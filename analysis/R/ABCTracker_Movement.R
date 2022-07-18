##########################################################################################################
## Author: GREG CHISM
## Date: JUNE 2022
## email: gchism@arizona.edu
## Project: Nest shape does not affect ant colony performance against a nest invader despite altered worker movement and communication
## Title: ABCTracker movement data processing, visualization, and analysis
##########################################################################################################

#######DESCRIPTION OF THE SCRIPT##########
# Movement raw data import and processing
# Speed comparisons & analyses
# Traffic jams/ interquartile lengths
# Distance to the entrance & movement speeds
# Distance interquartile lengths
# All associated visualizations and analyses

##########################################################################################################
# INSTALL & LOAD REQUIRED PACKAGES
##########################################################################################################
install.packages("pacman") # Download package with function to load multiple packaged at once

pacman::p_load(assertthat, # Loading required packages for code below. p_load() will download packages that aren't in system library
               data.table,
               filesstrings,
               forcats,
               janitor,
               gganimate,
               ggpubr,
               here,
               lme4,
               lmerTest,
               magrittr,
               MuMIn,
               readr,
               tidyverse,
               RColorBrewer)

##########################################################################################################
# LOAD THE REQUIRED DATA SETS
##########################################################################################################

# REFERENCE DATASETS
# All reference data sets needed for the code below

# Reference coordinates for distance to the nest entrance in each tube nest
AggnStudyRefCoords <- read.csv(here("analysis", "data", "ref_data", "AggnStudyTubeRefCoords.csv"))

# Reference coordinates for distance to the nest entrance in each circle nest
AggnStudyCircleRefCoords <- read.csv(here("analysis", "data", "ref_data", "AggnStudyCircleRefCoords.csv")) 

##########################################################################################################
# PROCESS ALL RAW ABCTRACKER OUTPUT DATA SETS
# Change raw column headers to a more compatible format
##########################################################################################################

# COLONY 5 
# INVADER ASSAY
# Import the data and change raw column headers to a more compatible format
Colony5CircleAggnT  <- read_csv(here("analysis", "data", "raw_data", "Colony5_2CircleOct_18AggnTest.csv"))

Colony5CircleAggnT <- setNames(Colony5CircleAggnT, c("ID", "Frames", "X", "Y", "Orientation",
                                                     "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "HeadX", "HeadY"))

Colony5TubeAggnT  <- read_csv(here("analysis", "data", "raw_data", "Colony5TubeAug_17AggnTNew.csv"))

Colony5TubeAggnT <- setNames(Colony5TubeAggnT, c("ID", "Frames", "X", "Y", "Orientation",
                                                 "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "HeadX", "HeadY"))
# Standardizing speeds as AntLengths / sec
# NOTE, this method is used for all data sets

# CIRCLE NEST
Colony5CircleAggn <- Colony5CircleAggnT %>%
  # Changing any values less than 0, which generally occur at the first frame
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), 
         # Creating the Colony, Nest, and Trial columns
         Colony = 5, Nest = "Circle", Trial = "Aggn",
         # Creating a seconds column using known frames/ sec
         Seconds = Frames / 23.97602398,
         # Calculating AntLengths / sec by dividing speed by the mean ant tracklet length
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  # Standardizing the length of the video (5-mins)
  filter(Seconds < 300) %>%
  # Removing the undesired columns
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# TUBE NEST
Colony5TubeAggn <- Colony5TubeAggnT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 5, Nest = "Tube", Trial = "Aggn",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300) %>%
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# Joining the final data sets
Colony5Aggn <- full_join(Colony5CircleAggn, Colony5TubeAggn)

# BASELINE ASSAY
Colony5CirclePreT <- read_csv(here("analysis", "data", "raw_data", "Colony5_2_Oct_17_AggnPreAM2.csv"))

Colony5CirclePreT <- setNames(Colony5CirclePreT, c("ID", "Frames", "X", "Y", "Orientation",
                                                   "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "HeadX", "HeadY"))

Colony5TubePreT <- read_csv(here("analysis", "data", "raw_data", "Colony5_24Tube_Oct_17_AggnExpPM.csv"))

Colony5TubePreT <- setNames(Colony5TubePreT, c("ID", "Frames", "X", "Y", "Orientation",
                                               "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "HeadX", "HeadY"))
# CIRCLE NEST
Colony5CirclePre <- Colony5CirclePreT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 5, Nest = "Circle", Trial = "Pre",
         # NOTE that the frame rate here is different 
         # Some videos were imported with this frame rate for an unknown reason
         Seconds = Frames / 29.97,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300 ) %>%
dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# TUBE NEST
Colony5TubePre <- Colony5TubePreT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 5, Nest = "Tube", Trial = "Pre",
         Seconds = Frames / 29.97,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300) %>%
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# Joining the final data sets
Colony5Pre <- full_join(Colony5CirclePre, Colony5TubePre)

# COLONY 6 
# INVADER ASSAY
Colony6CircleAggnT  <- read_csv(here("analysis", "data", "raw_data", "Colony6_3CircleOct_18AggnT.csv"))

Colony6CircleAggnT <- setNames(Colony6CircleAggnT, c("ID", "Frames", "X", "Y", "Orientation",
                                                     "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "HeadX", "HeadY"))

Colony6TubeAggnT  <- read_csv(here("analysis", "data", "raw_data", "Colony6TubeOct_17AggnT.csv"))

Colony6TubeAggnT <- setNames(Colony6TubeAggnT, c("ID", "Frames", "X", "Y", "Orientation",
                                                 "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "HeadX", "HeadY"))
# CIRCLE NEST
Colony6CircleAggn <- Colony6CircleAggnT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 6, Nest = "Circle", Trial = "Aggn",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300) %>%
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# TUBE NEST
Colony6TubeAggn <- Colony6TubeAggnT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 6, Nest = "Tube", Trial = "Aggn",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300) %>%
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# Joining the final data sets
Colony6Aggn <- full_join(Colony6CircleAggn, Colony6TubeAggn)

# BASELINE ASSAY
Colony6CirclePreT <- read_csv(here("analysis", "data", "raw_data", "Colony6_3_Circle_Oct_17_AggnPreAM.csv"))

Colony6CirclePreT <- setNames(Colony6CirclePreT, c("ID", "Frames", "X", "Y", "Orientation",
                                                   "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "HeadX", "HeadY"))

Colony6TubePreT <- read_csv(here("analysis", "data", "raw_data", "Colony6_27_Oct_17_AggnPreAM.csv"))

Colony6TubePreT <- setNames(Colony6TubePreT, c("ID", "Frames", "X", "Y", "Orientation",
                                               "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "HeadX", "HeadY"))
# CIRCLE NEST
Colony6CirclePre <- Colony6CirclePreT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 6, Nest = "Circle", Trial = "Pre",
         Seconds = Frames / 29.97,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300) %>%
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# TUBE NEST
Colony6TubePre <- Colony6TubePreT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 6, Nest = "Tube",Trial = "Pre",
         Seconds = Frames / 29.97,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300) %>%
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# Joining the final data sets
Colony6Pre <- full_join(Colony6CirclePre, Colony6TubePre)

# COLONY 7 
# INVADER ASSAY
Colony7CircleAggnT  <- read_csv(here("analysis", "data", "raw_data", "Colony7Circle_AggnT.csv"))

Colony7CircleAggnT <- setNames(Colony7CircleAggnT, c("ID","Frames","X","Y","Orientation",
                                                     "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony7TubeAggnT  <- read_csv(here("analysis", "data", "raw_data", "Colony7Tube_AggnT.csv")) 

Colony7TubeAggnT <- setNames(Colony7TubeAggnT, c("ID","Frames","X","Y","Orientation",
                                                 "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))
# CIRCLE NEST
Colony7CircleAggn <- Colony7CircleAggnT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 7, Nest = "Circle", Trial = "Aggn",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300) %>%
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# TUBE NEST
Colony7TubeAggn <- Colony7TubeAggnT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 7, Nest = "Tube", Trial = "Aggn",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300) %>%
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# Joining the final data sets
Colony7Aggn <- full_join(Colony7CircleAggn, Colony7TubeAggn)

# BASELINE ASSAY
Colony7CirclePreT <- read_csv(here("analysis", "data", "raw_data", "Colony7_Circle_5_Oct_17_VidAMPre.csv"))

Colony7CirclePreT <- setNames(Colony7CirclePreT, c("ID","Frames","X","Y","Orientation",
                                                   "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony7TubePreT <- read_csv(here("analysis", "data", "raw_data", "Colony7_23_Oct_17_AggnPreAM.csv"))

Colony7TubePreT <- setNames(Colony7TubePreT, c("ID","Frames","X","Y","Orientation",
                                               "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

# CIRCLE NEST
Colony7CirclePre <- Colony7CirclePreT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 7, Nest = "Circle", Trial = "Pre",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300) %>%
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# TUBE NEST
Colony7TubePre <- Colony7TubePreT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 7, Nest = "Tube", Trial = "Pre",
         Seconds = Frames / 29.97,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300) %>%
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# Joining the final data sets
Colony7Pre <- full_join(Colony7CirclePre, Colony7TubePre)

# COLONY 8
# INVADER ASSAY
Colony8CircleAggnT  <- read_csv(here("analysis", "data", "raw_data", "Colony8CircleJune_18AggnT.csv")) 

Colony8CircleAggnT <- setNames(Colony8CircleAggnT, c("ID","Frames","X","Y","Orientation",
                                                   "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony8TubeAggnT  <- read_csv(here("analysis", "data", "raw_data", "Colony8TubeJune_18AggnT.csv")) 

Colony8TubeAggnT <- setNames(Colony8TubeAggnT, c("ID","Frames","X","Y","Orientation",
                                               "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))
# CIRCLE NEST
Colony8CircleAggn <- Colony8CircleAggnT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 8, Nest = "Circle", Trial = "Aggn",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300) %>%
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# TUBE NEST
Colony8TubeAggn <- Colony8TubeAggnT%>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 8, Nest = "Tube", Trial = "Aggn",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300 ) %>%
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# Joining the final data sets
Colony8Aggn <- full_join(Colony8CircleAggn, Colony8TubeAggn)

# BASELINE ASSAY 
Colony8CirclePreT <- read_csv(here("analysis", "data", "raw_data", "Colony8_25_June_18_AggnPreAM.csv")) 

Colony8CirclePreT <- setNames(Colony8CirclePreT, c("ID","Frames","X","Y","Orientation",
                                                 "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony8TubePreT <- read_csv(here("analysis", "data", "raw_data", "Colony8_7_June_18_AggnPreAM.csv"))

Colony8TubePreT <- setNames(Colony8TubePreT, c("ID","Frames","X","Y","Orientation",
                                             "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

# CIRCLE NEST 
Colony8CirclePre <- Colony8CirclePreT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 8, Nest = "Circle", Trial = "Pre",
         Seconds = Frames / 29.97,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300) %>%
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# TUBE NEST
Colony8TubePre <- Colony8TubePreT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 8, Nest = "Tube", Trial = "Pre",
         Seconds = Frames / 29.97,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300) %>%
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# Joining the final data sets
Colony8Pre <- full_join(Colony8CirclePre, Colony8TubePre)

# COLONY 9
# INVADER ASSAY
Colony9CircleAggnT  <- read_csv(here("analysis", "data", "raw_data", "Colony9CircleJune_18AggnT.csv"))

Colony9CircleAggnT <- setNames(Colony9CircleAggnT, c("ID","Frames","X","Y","Orientation",
                                                     "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony9TubeAggnT  <- read_csv(here("analysis", "data", "raw_data", "Colony9TubeJune_18AggnT.csv"))

Colony9TubeAggnT <- setNames(Colony9TubeAggnT, c("ID","Frames","X","Y","Orientation",
                                                 "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

# CIRCLE NEST 
Colony9CircleAggn <- Colony9CircleAggnT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 9, Nest = "Circle", Trial = "Aggn",
         Framerate = 23.97602398, Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300 ) %>%
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# TUBE NEST 
Colony9TubeAggn <- Colony9TubeAggnT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 9, Nest = "Tube", Trial = "Aggn",
         Framerate = 23.97602398, Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300 ) %>% 
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# Joining the final data sets
Colony9Aggn <- full_join(Colony9CircleAggn, Colony9TubeAggn)

# BASELINE ASSAY 
Colony9CirclePreT <- read_csv(here("analysis", "data", "raw_data", "Colony9CirclePre.csv"))

Colony9CirclePreT <- setNames(Colony9CirclePreT, c("ID","Frames","X","Y","Orientation",
                                                   "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))
Colony9TubePreT <- read_csv(here("analysis", "data", "raw_data", "Colony9_Tube_6_June_18_AggnP.csv"))

Colony9TubePreT <- setNames(Colony9TubePreT, c("ID","Frames","X","Y","Orientation",
                                               "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

# CIRCLE NEST
Colony9CirclePre <- Colony9CirclePreT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 9, Nest = "Circle", Trial = "Pre",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300 ) %>% 
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# TUBE NEST
Colony9TubePre <- Colony9TubePreT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 9, Nest = "Tube", Trial = "Pre",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300 ) %>% 
  dplyr::select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

# Joining the final data sets
Colony9Pre <- full_join(Colony9CirclePre, Colony9TubePre)

# COLONY 11
# INVADER ASSAY

unzip(here("analysis", "data", "raw_data", "Colony11CircleJune_18AggnT.csv.zip")) 

filesstrings::file.move("Colony11CircleJune_18AggnT.csv", here("analysis", "data", "raw_data"))

Colony11CircleAggnT  <- read_csv(here("analysis", "data", "raw_data", "Colony11CircleJune_18AggnT.csv"))%>%
  dplyr::select(-c("Tag", "Completed"))

Colony11CircleAggnT <- setNames(Colony11CircleAggnT, c("ID","Frames","X","Y","Orientation",
                                                     "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony11TubeAggnT  <- read_csv(here("analysis", "data", "raw_data", "Colony11TubeJune_18AggnT.csv")) 

Colony11TubeAggnT <- setNames(Colony11TubeAggnT, c("ID","Frames","X","Y","Orientation",
                                                 "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

# CIRCLE NEST 
Colony11CircleAggn <- Colony11CircleAggnT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 11, Nest = "Circle", Trial = "Aggn",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300) %>% 
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# TUBE NEST
Colony11TubeAggn <- Colony11TubeAggnT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 11, Nest = "Tube", Trial = "Aggn",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300 & AntLength.sec < 10) %>% 
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# Joining the final data sets
Colony11Aggn <- full_join(Colony11CircleAggn, Colony11TubeAggn)

# BASELINE ASSAY
unzip(here("analysis", "data", "raw_data", "Colony11_Circle_28_June_18_AggnPreAM.csv.zip")) 

filesstrings::file.move("Colony11_Circle_28_June_18_AggnPreAM.csv", here("analysis", "data", "raw_data"))

Colony11CirclePreT <- read_csv(here("analysis", "data", "raw_data", "Colony11_Circle_28_June_18_AggnPreAM.csv"))

Colony11CirclePreT <- setNames(Colony11CirclePreT, c("ID","Frames","X","Y","Orientation",
                                                  "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))
Colony11TubePreT <- read_csv(here("analysis", "data", "raw_data", "Colony11_10_June_18_AggnPreAM_2.csv"))

Colony11TubePreT <- setNames(Colony11TubePreT, c("ID","Frames","X","Y","Orientation",
                                               "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

# CIRCLE NEST 
Colony11CirclePre <- Colony11CirclePreT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 11, Nest = "Circle", Trial = "Pre",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300 ) %>% 
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# TUBE NEST
Colony11TubePre <- Colony11TubePreT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 11, Nest = "Tube", Trial = "Pre",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300 ) %>% 
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# Joining the final data sets
Colony11Pre <- full_join(Colony11TubePre, Colony11CirclePre)

# COLONY 13
# INVADER ASSAY
Colony13CircleAggnT  <- read_csv(here("analysis", "data", "raw_data", "Colony13CircleJune_18AggnT.csv"))

Colony13CircleAggnT <- setNames(Colony13CircleAggnT, c("ID","Frames","X","Y","Orientation",
                                                       "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony13TubeAggnT  <- read_csv(here("analysis", "data", "raw_data", "Colony13TubeJune_18AggnT.csv")) 

Colony13TubeAggnT <- setNames(Colony13TubeAggnT, c("ID","Frames","X","Y","Orientation",
                                                   "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

# CIRCLE NEST
Colony13CircleAggn <- Colony13CircleAggnT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 13, Nest = "Circle", Trial = "Aggn",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300 ) %>% 
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# TUBE NEST
Colony13TubeAggn <- Colony13TubeAggnT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 13, Nest = "Tube", Trial = "Aggn",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s /mean(SizeLeng.px)) %>%
  filter(Seconds < 300 ) %>% 
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# Joining the final data sets
Colony13Aggn <- full_join(Colony13CircleAggn, Colony13TubeAggn)

#Pre assay
Colony13CirclePreT <- read_csv(here("analysis", "data", "raw_data", "Colony13CirclePre.csv"))

Colony13CirclePreT <- setNames(Colony13CirclePreT, c("ID","Frames","X","Y","Orientation",
                                                     "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))
Colony13TubePreT <- read_csv(here("analysis", "data", "raw_data", "Colony13_Tube_29_June_18_AggnPre.csv"))

Colony13TubePreT <- setNames(Colony13TubePreT, c("ID","Frames","X","Y","Orientation",
                                                 "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

# CIRCLE NEST
Colony13CirclePre <- Colony13CirclePreT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 13, Nest = "Circle", Trial = "Pre",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300 ) %>% 
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# TUBE NEST
Colony13TubePre <- Colony13TubePreT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 13, Nest = "Tube", Trial = "Pre",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300 ) %>% 
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# Joining the final data sets
Colony13Pre <- full_join(Colony13CirclePre, Colony13TubePre)

# COLONY 17
# INVADER ASSAY
Colony17CircleAggnT  <- read_csv(here("analysis", "data", "raw_data", "Colony17CircleAug_18AggnT.csv"))

Colony17CircleAggnT <- setNames(Colony17CircleAggnT, c("ID","Frames","X","Y","Orientation",
                                                       "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony17TubeAggnT  <- read_csv(here("analysis", "data", "raw_data", "Colony17TubeJuly_18AggnT.csv"))

Colony17TubeAggnT <- setNames(Colony17TubeAggnT, c("ID","Frames","X","Y","Orientation",
                                                   "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

# CIRCLE NEST
Colony17CircleAggn <- Colony17CircleAggnT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 17, Nest = "Circle", Trial = "Aggn",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300 ) %>% 
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# TUBE NEST
Colony17TubeAggn <- Colony17TubeAggnT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 17, Nest = "Tube", Trial = "Aggn",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300 ) %>% 
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# Joining the final data sets 
Colony17Aggn <- full_join(Colony17CircleAggn, Colony17TubeAggn)

# BASELINE ASSAY
Colony17CirclePreT <- read_csv(here("analysis", "data", "raw_data", "Colony17_Circle_12_Aug_18_AggnPreAM.csv"))

Colony17CirclePreT <- setNames(Colony17CirclePreT, c("ID","Frames","X","Y","Orientation",
                                                     "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony17TubePreT <- read_csv(here("analysis", "data", "raw_data", "Colony17_21_July_18_AggnPrePM_2.csv"))

Colony17TubePreT <- setNames(Colony17TubePreT, c("ID","Frames","X","Y","Orientation",
                                                     "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

# CIRCLE NEST
Colony17CirclePre <- Colony17CirclePreT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 17, Nest = "Circle", Trial = "Pre",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300 ) %>% 
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# TUBE NEST
Colony17TubePre <- Colony17TubePreT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 17, Nest = "Tube", Trial = "Pre",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300 ) %>% 
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# Joining the final data sets 
Colony17Pre <- full_join(Colony17CirclePre, Colony17TubePre)

# COLONY18
# INVADER ASSAY
Colony18CircleAggnT  <- read_csv(here("analysis", "data", "raw_data", "Colony18CircleAug_18AggnT.csv")) 

Colony18CircleAggnT <- setNames(Colony18CircleAggnT, c("ID","Frames","X","Y","Orientation",
                                                       "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony18TubeAggnT  <- read_csv(here("analysis", "data", "raw_data", "Colony18TubeJuly_18AggnT.csv"))

Colony18TubeAggnT <- setNames(Colony18TubeAggnT, c("ID","Frames","X","Y","Orientation",
                                                   "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

# CIRCLE NEST
Colony18CircleAggn <- Colony18CircleAggnT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 18, Nest = "Circle", Trial = "Aggn",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300 ) %>% 
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# TUBE NEST 
Colony18TubeAggn <- Colony18TubeAggnT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 18, Nest = "Tube", Trial = "Aggn",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300 ) %>% 
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# Joining the final data sets 
Colony18Aggn <- full_join(Colony18CircleAggn, Colony18TubeAggn)

# BASELINE ASSAY
Colony18CirclePreT <- read_csv(here("analysis", "data", "raw_data", "Colony18_Circle_14_Aug_18_AggnPreAM.csv"))

Colony18CirclePreT <- setNames(Colony18CirclePreT, c("ID","Frames","X","Y","Orientation",
                                                     "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony18TubePreT <- read_csv(here("analysis", "data", "raw_data", "Colony18TubePreVideo.csv")) 

Colony18TubePreT <- setNames(Colony18TubePreT, c("ID","Frames","X","Y","Orientation",
                                                 "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

# CIRCLE NEST
Colony18CirclePre <- Colony18CirclePreT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 18, Nest = "Circle", Trial = "Pre",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300 ) %>% 
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# TUBE NEST
Colony18TubePre <- Colony18TubePreT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 18, Nest = "Tube", Trial = "Pre",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px),
         # This video was filmed upside down, so the coordinates and orientation needed to be corrected
         X = (X * -1) + 1920, Y = (Y * -1) + 1080, HeadX = (HeadX * -1) + 1920, HeadY = (HeadY * -1) + 1080,
         # Orientation isn't needed, but is corrected below for potential future use in a different study
         Orientation = ifelse((Orientation + 180) > 360, Orientation - 180, Orientation + 180)) %>%
  filter(Seconds < 300 ) %>% 
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# Joining the final data sets 
Colony18Pre <- full_join(Colony18CirclePre, Colony18TubePre)

# COLONY 20
# INVADER ASSAY
Colony20CircleAggnT <- read_csv(here("analysis", "data", "raw_data", "Colony20CircleAug_18AggnT.csv"))

Colony20CircleAggnT <- setNames(Colony20CircleAggnT, c("ID","Frames","X","Y","Orientation",
                                                     "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony20TubeAggnT<- read_csv(here("analysis", "data", "raw_data", "Colony20TubeAug_18AggnT.csv")) 

Colony20TubeAggnT <- setNames(Colony20TubeAggnT, c("ID","Frames","X","Y","Orientation",
                                                 "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

# CIRCLE NEST
Colony20CircleAggn <- Colony20CircleAggnT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 20, Nest = "Circle", Trial = "Aggn",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300 ) %>% 
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# TUBE NEST
Colony20TubeAggn <- Colony20TubeAggnT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 20, Nest = "Tube", Trial = "Aggn",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300 ) %>% 
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# Joining the final data sets 
Colony20Aggn <- full_join(Colony20CircleAggn, Colony20TubeAggn)

# BASELINE ASSAY
Colony20CirclePreT <- read_csv(here("analysis", "data", "raw_data", "Colony20_18Aug20_CircPre.csv")) 

Colony20CirclePreT <- setNames(Colony20CirclePreT, c("ID","Frames","X","Y","Orientation",
                                                   "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony20TubePreT <- read_csv(here("analysis", "data", "raw_data", "Colony20_Tube_1_Aug_18_AggnPreAM.csv"))

Colony20TubePreT <- setNames(Colony20TubePreT, c("ID","Frames","X","Y","Orientation",
                                               "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

# CIRCLE NEST
Colony20CirclePre <- Colony20CirclePreT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 20, Nest = "Circle", Trial = "Pre",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300 ) %>% 
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# TUBE NEST
Colony20TubePre <- Colony20TubePreT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 20, Nest = "Tube", Trial = "Pre",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300 ) %>% 
  dplyr::select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

# Joining the final data sets 
Colony20Pre <- full_join(Colony20CirclePre, Colony20TubePre)

##########################################################################################################
# TIME BINS (SECONDS)
# Creating the bins used throughout the code. These bins allow us to average worker speeds within given time bins
##########################################################################################################

# SECONDS BINS
# TWO SECOND BINS
TwoSecsList <- Colony20Aggn %>% # Reference empirical data set 
  ungroup() %>% # Remove any groupings
  dplyr::select(Seconds) %>% # Select only the Seconds column
  distinct() %>% # Remove any duplicates
  drop_na() %>% # Drop any NAs
  mutate(Seconds = cut_width(Seconds, width = 2, boundary = 0)) %>% # Create a sequence from the Seconds column at every 2 seconds, where the value is a bin (e.g. 2,4)
  distinct() %>% # Remove any duplicates
  mutate(obs = 1:n(), # Create a column that is the row name (1, 2, 3, 4,...)
         Secs = obs * 2, # Create a column that is the row name * 2, so that the column represents the seconds value only and not a bin
         Seconds = gsub("\\[|\\]", "", Seconds), # Remove the extra [ that forms around the Seconds column when you use cut_width
         Seconds = gsub("\\(|\\)", "", Seconds)) %>% # Remove the extra ( that forms around the Seconds column when you use cut_width
  dplyr::select(-c(obs)) # Remove the obs column

# TWENTY SECOND BINS
TwentySecBin <- Colony20Aggn %>%
  ungroup() %>% # Remove any groupings
  dplyr::select(Seconds) %>% # Select only the Seconds column
  distinct() %>% # Remove any duplicates
  drop_na() %>% # Drop any NAs
  mutate(Seconds = cut_width(Seconds, width = 20, boundary = 0)) %>% # Create a sequence from the Seconds column at every 20 seconds, where the value is a bin (e.g. 20,40)
  distinct() %>%
  mutate(obs = 1:n(), # Create a column that is the row name (1, 2, 3, 4,...)
         Secs20 = obs * 20, # Create a column that is the row name * 20, so that the column represents the seconds value only and not a bin
         Seconds = gsub("\\[|\\]", "", Seconds), # Remove the extra [ that forms around the Seconds column when you use cut_width
         Seconds = gsub("\\(|\\)", "", Seconds)) %>% # Remove the extra ( that forms around the Seconds column when you use cut_width
  dplyr::select(-c(obs)) # Remove the obs column

# SCALED DISTANCE TO ENTRANCE 
# Calculating each workers scaled distance to the nest entrance for every individual worker at every time step 
# The scale is the farthest distance to the nest entrance for each nest shape
# Scaled distances are then binned into 0.05 scaled distance bins
# There are functions for every nest shape and assay combination because some data sets are very large and combining all of the functions could crash a computer

# NOTE: This first distance to the nest entrance function is to generate a data frame called "FiveDistList".
DistanceCoordsFunctionTrackerRef<-function(data.table){
  TubeCoords <- AggnStudyRefCoords %>% # Reference data set for eight even area bins
    filter(Colony == "11" & Trial == "Aggn" & Nest == "Tube") %>% # Filter out only tube nest aggression assay references
    dplyr::select(c(Coord, XREF, YREF)) # Select only desired columns 
  
  # X axis references
  XCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(YREF)) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF1 = as.numeric(XCoords[,1]) # X axis reference from column 1 from above
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  XREF3 = as.numeric(XCoords[,3]) # X axis reference from column 3 from above
  XREF4 = as.numeric(XCoords[,4]) # X axis reference from column 4 from above
  XREF5 = as.numeric(XCoords[,5]) # X axis reference from column 5 from above
  XREF6 = as.numeric(XCoords[,6]) # X axis reference from column 6 from above
  XREF7 = as.numeric(XCoords[,7]) # X axis reference from column 7 from above
  
  # Y axis references
  YCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(XREF)) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # Y axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # Y axis reference from column 2 from above
  YREF3 = as.numeric(YCoords[,3]) # Y axis reference from column 3 from above
  YREF4 = as.numeric(YCoords[,4]) # Y axis reference from column 4 from above
  YREF5 = as.numeric(YCoords[,5]) # Y axis reference from column 5 from above
  YREF6 = as.numeric(YCoords[,6]) # Y axis reference from column 6 from above
  YREF7 = as.numeric(YCoords[,7]) # Y axis reference from column 7 from above
  
  # Width of the tube nest
  Width = YREF2 - YREF3
  
  # Reference distances within the tube nest
  Dist1 = sqrt(((XREF1 - XREF2)^2) + ((YREF1 - YREF2)^2)) # Distance to the entrance from corner entering bin 1 from bin 2
  Dist2 = sqrt(((XREF2 - XREF3)^2) + ((YREF2 - YREF3)^2)) + Dist1 # Distance to the entrance from bin 3 entering bin 2
  Dist3 = sqrt(((XREF4 - XREF3)^2) + ((YREF3 - YREF4)^2)) + Dist2 # Distance to the entrance from bin 4, 5, 6 entering bin 3
  Dist4 = sqrt(((XREF5 - XREF4)^2) + ((YREF5 - YREF4)^2)) + Dist3 # Distance to the entrance from bin 7 entering bin 6
  Dist5 = sqrt(((XREF6 - XREF5)^2) + ((YREF6 - YREF5)^2)) + Dist4 # Distance to the entrance from bin 8 entering bin 7
  MaxDist = sqrt(((XREF6 - XREF7)^2) + ((YREF6 - YREF7)^2)) + Dist5 # Distance from the back of the nest to the entrance
  
  Colony11TubeAggnDistRef <<- data.table %>% # Data table to run through the function
    filter(Nest == "Tube") %>% # Filter out tube nest coordinates
    # Coordinate bin assignment
    mutate(X1 = ifelse(X >= XREF2 & X <= (XREF2 + (1.25 * Width)) & Y >= YREF3, # If the x axiss of each coordinate is in bin 1
                    "YES", "NO"), 
           X2 = ifelse(X < XREF2 & Y >= YREF3, # If the x axis of each coordinate is in bin 2
                    "YES", "NO"),
           X3 = ifelse(X <= XREF4 & Y < YREF3, # If the x axis of each coordinate is in bin 3
                    "YES", "NO"),
           X4 = ifelse(X > XREF4 & Y <= YREF5, # If the x axis of each coordinate is in bins 4, 5, or 6
                    "YES", "NO"),
           X5 = ifelse(X >= XREF6 & Y > YREF5, # If the x axis of each coordinate is in bin 7
                    "YES", "NO"),
           X6 = ifelse(X >= XREF7 & Y >= YREF7, # If the x axis of each coordinate is in bin 8
                    "YES", "NO"),
           Y1 = ifelse(X1 == "YES", "YES", "NO"), # If the coordinate qualified under X1
           Y2 = ifelse(X2 == "YES", "YES", "NO"), # If the coordinate qualified under X2
           Y3 = ifelse(X3 == "YES", "YES", "NO"), # If the coordinate qualified under X3
           Y4 = ifelse(X4 == "YES", "YES", "NO"), # If the coordinate qualified under X4
           Y5 = ifelse(X5 == "YES", "YES", "NO"), # If the coordinate qualified under X5
           Y6 = ifelse(X6 == "YES", "YES", "NO"), # If the coordinate qualified under X6
           # X axis distances
           DistanceX = ifelse(X1 == "YES", # Bin 1
                              X - XREF1,
                              ifelse(X2 == "YES", # Bin 2
                                     XREF2 - X,
                                     ifelse(X3 == "YES", # Bin 3
                                            XREF3 - X,
                                            ifelse(X4 == "YES", # Bins 4-6
                                                   X - XREF4,
                                                   ifelse(X5 == "YES", # Bin 7
                                                          X - XREF5,
                                                          ifelse(X6 == "YES", # Bin 8
                                                                 XREF6 - X,
                                                                 NA)))))), # Else NA
           # Y axis distances
           DistanceY = ifelse(Y1 == "YES", # Bin 1
                              YREF1 - Y,
                              ifelse(Y2 == "YES", # Bin 2
                                     YREF2 - Y,
                                     ifelse(Y3 == "YES" | Y4 == "YES" & X < XREF4, # Bin 3, or bin 4 with x coordinates less than the bin's x axis reference
                                            YREF3 - Y,
                                            ifelse(Y4 == "YES", # Bin 4-6
                                                   YREF4 - Y,
                                                   ifelse(Y5 == "YES", # Bin 7
                                                          Y - YREF5,
                                                          ifelse(Y6 == "YES", # Bin 8
                                                                 YREF6 - Y,
                                                                 NA)))))),
           # Shortest distances from the nest entrance
           # Only bin 1 is a direct line to the entrance, every other bin is the shortest distance to the closest bin to the entrance + reference distance for that bin to the entrance
           PythagDist = ifelse(X1 == "YES" & Y1 == "YES", # Bin 1
                             sqrt((DistanceX^2) + (DistanceY^2)),
                             ifelse(X2 == "YES" & Y2 == "YES", # Bin 2
                                    sqrt((DistanceX^2) + (DistanceY^2)) + Dist1,
                                    ifelse(X3 == "YES" & Y3 == "YES", # Bin 3
                                           sqrt((DistanceX^2) + (DistanceY^2)) + Dist2,
                                           ifelse(X4 == "YES" & Y4 == "YES", # Bin 4-6
                                                  sqrt((DistanceX^2) + (DistanceY^2)) + Dist3, 
                                                  ifelse(X5 == "YES" & Y5 == "YES", # Bin 7
                                                         sqrt((DistanceX^2) + (DistanceY^2)) + Dist4,
                                                         ifelse(X6 == "YES" & Y6 == "YES", # Bin 8
                                                                sqrt((DistanceX^2) + (DistanceY^2)) + Dist5,
                                                                NA)))))),
           # Scaling distances by the max possible distance
           ScaledDist = PythagDist / MaxDist,
           # If any distance is greater than 1 (precautionary), then it's assigned a 1
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>%
    ungroup() %>% # Ungroup the data
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) # Remove the extra [ that forms around the DistBin column when you use cut_width
}

# Run the reference producing distance to the nest entrance function for the Colony11TubeAggn data set
DistanceCoordsFunctionTrackerRef(Colony11TubeAggn)

# DISTANCE TO THE NEST ENTRANCE BINS
# Creating the distance bin we used throughout the code. These bins allow us to average worker speeds within given distance bins from the nest entrance to the very back of the nest
# The code requires a reference data set. We chose one with workers throughout the entire nest (above) 
FiveDistList <- Colony11TubeAggnDistRef %>% 
  ungroup() %>% # ungroup the data set                        
  dplyr::select(ScaledDist) %>% # Select only the desired column
  distinct() %>% # Removing duplicate values
  mutate(DistBin = cut_width(ScaledDist, width = 0.05,boundary = 0)) %>% # Creating a set of bins by cutting scaled distance to the entrance (0 - 1) by 0.05 increments.
  dplyr::select(DistBin) %>% # Selecting only the distance bin column. 
  distinct() %>% # Removing duplicate values
  drop_na() %>% # Dropping any NA values 
  arrange(DistBin) %>% # Arranging distance bins from smallest to largest. 
  mutate(obs = 1:n(), Distance = 0.05 * obs, # Creating references for the bins
         DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
         DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width
  dplyr::select(-c(obs)) # Remove the obs reference column

# DISTANCE TO THE NEST ENTRANCE 
# Code to calculate each workers scaled distance to the nest entrance at every two-second bin (see above)
# NOTE: This code is duplicated for every colony

# COLONY 5
# TUBE NEST
# INVADER
DistanceCoordsFunctionTrackerTubeAggn5 <- function(data.table){
  TubeCoords <- AggnStudyRefCoords %>% # Reference data set for eight even area bins
    filter(Colony == "5" & Trial == "Aggn" & Nest == "Tube") %>% # Filter out only tube nest aggression assay references
    dplyr::select(c(Coord, XREF, YREF)) # Select only desired columns 
  
  # X axis references
  XCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(YREF)) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF1 = as.numeric(XCoords[,1]) # X axis reference from column 1 from above
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  XREF3 = as.numeric(XCoords[,3]) # X axis reference from column 3 from above
  XREF4 = as.numeric(XCoords[,4]) # X axis reference from column 4 from above
  XREF5 = as.numeric(XCoords[,5]) # X axis reference from column 5 from above
  XREF6 = as.numeric(XCoords[,6]) # X axis reference from column 6 from above
  XREF7 = as.numeric(XCoords[,7]) # X axis reference from column 7 from above
  
  # Y axis references
  YCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(XREF)) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # Y axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # Y axis reference from column 2 from above
  YREF3 = as.numeric(YCoords[,3]) # Y axis reference from column 3 from above
  YREF4 = as.numeric(YCoords[,4]) # Y axis reference from column 4 from above
  YREF5 = as.numeric(YCoords[,5]) # Y axis reference from column 5 from above
  YREF6 = as.numeric(YCoords[,6]) # Y axis reference from column 6 from above
  YREF7 = as.numeric(YCoords[,7]) # Y axis reference from column 7 from above
  
  # Width of the tube nest
  Width = YREF2 - YREF3
  
  # Reference distances within the tube nest
  Dist1 = sqrt(((XREF1 - XREF2)^2) + ((YREF1 - YREF2)^2)) # Distance to the entrance from corner entering bin 1 from bin 2
  Dist2 = sqrt(((XREF2 - XREF3)^2) + ((YREF2 - YREF3)^2)) + Dist1 # Distance to the entrance from bin 3 entering bin 2
  Dist3 = sqrt(((XREF4 - XREF3)^2) + ((YREF3 - YREF4)^2)) + Dist2 # Distance to the entrance from bin 4, 5, 6 entering bin 3
  Dist4 = sqrt(((XREF5 - XREF4)^2) + ((YREF5 - YREF4)^2)) + Dist3 # Distance to the entrance from bin 7 entering bin 6
  Dist5 = sqrt(((XREF6 - XREF5)^2) + ((YREF6 - YREF5)^2)) + Dist4 # Distance to the entrance from bin 8 entering bin 7
  MaxDist = sqrt(((XREF6 - XREF7)^2) + ((YREF6 - YREF7)^2)) + Dist5 # Distance from the back of the nest to the entrance
  
  Colony5TubeAggnDist <<- data.table %>% # Data table to run through the function
    filter(Nest == "Tube") %>% # Filter out tube nest coordinates
    # Coordinate bin assignment
    mutate(X1 = ifelse(X >= XREF2 & X <= (XREF2 + (1.25 * Width)) & Y >= YREF3, # If the x axiss of each coordinate is in bin 1
                       "YES", "NO"), 
           X2 = ifelse(X < XREF2 & Y >= YREF3, # If the x axis of each coordinate is in bin 2
                       "YES", "NO"),
           X3 = ifelse(X <= XREF4 & Y < YREF3, # If the x axis of each coordinate is in bin 3
                       "YES", "NO"),
           X4 = ifelse(X > XREF4 & Y <= YREF5, # If the x axis of each coordinate is in bins 4, 5, or 6
                       "YES", "NO"),
           X5 = ifelse(X >= XREF6 & Y > YREF5, # If the x axis of each coordinate is in bin 7
                       "YES", "NO"),
           X6 = ifelse(X >= XREF7 & Y >= YREF7, # If the x axis of each coordinate is in bin 8
                       "YES", "NO"),
           Y1 = ifelse(X1 == "YES", "YES", "NO"), # If the coordinate qualified under X1
           Y2 = ifelse(X2 == "YES", "YES", "NO"), # If the coordinate qualified under X2
           Y3 = ifelse(X3 == "YES", "YES", "NO"), # If the coordinate qualified under X3
           Y4 = ifelse(X4 == "YES", "YES", "NO"), # If the coordinate qualified under X4
           Y5 = ifelse(X5 == "YES", "YES", "NO"), # If the coordinate qualified under X5
           Y6 = ifelse(X6 == "YES", "YES", "NO"), # If the coordinate qualified under X6
           # X axis distances
           DistanceX = ifelse(X1 == "YES", # Bin 1
                              X - XREF1,
                              ifelse(X2 == "YES", # Bin 2
                                     XREF2 - X,
                                     ifelse(X3 == "YES", # Bin 3
                                            XREF3 - X,
                                            ifelse(X4 == "YES", # Bins 4-6
                                                   X - XREF4,
                                                   ifelse(X5 == "YES", # Bin 7
                                                          X - XREF5,
                                                          ifelse(X6 == "YES", # Bin 8
                                                                 XREF6 - X,
                                                                 NA)))))), # Else NA
           # Y axis distances
           DistanceY = ifelse(Y1 == "YES", # Bin 1
                              YREF1 - Y,
                              ifelse(Y2 == "YES", # Bin 2
                                     YREF2 - Y,
                                     ifelse(Y3 == "YES" | Y4 == "YES" & X < XREF4, # Bin 3, or bin 4 with x coordinates less than the bin's x axis reference
                                            YREF3 - Y,
                                            ifelse(Y4 == "YES", # Bin 4-6
                                                   YREF4 - Y,
                                                   ifelse(Y5 == "YES", # Bin 7
                                                          Y - YREF5,
                                                          ifelse(Y6 == "YES", # Bin 8
                                                                 YREF6 - Y,
                                                                 NA)))))),
           # Shortest distances from the nest entrance
           # Only bin 1 is a direct line to the entrance, every other bin is the shortest distance to the closest bin to the entrance + reference distance for that bin to the entrance
           PythagDist = ifelse(X1 == "YES" & Y1 == "YES", # Bin 1
                               sqrt((DistanceX^2) + (DistanceY^2)),
                               ifelse(X2 == "YES" & Y2 == "YES", # Bin 2
                                      sqrt((DistanceX^2) + (DistanceY^2)) + Dist1,
                                      ifelse(X3 == "YES" & Y3 == "YES", # Bin 3
                                             sqrt((DistanceX^2) + (DistanceY^2)) + Dist2,
                                             ifelse(X4 == "YES" & Y4 == "YES", # Bin 4-6
                                                    sqrt((DistanceX^2) + (DistanceY^2)) + Dist3, 
                                                    ifelse(X5 == "YES" & Y5 == "YES", # Bin 7
                                                           sqrt((DistanceX^2) + (DistanceY^2)) + Dist4,
                                                           ifelse(X6 == "YES" & Y6 == "YES", # Bin 8
                                                                  sqrt((DistanceX^2) + (DistanceY^2)) + Dist5,
                                                                  NA)))))),
           # Scaling distances by the max possible distance
           ScaledDist = PythagDist / MaxDist,
           # If any distance is greater than 1 (precautionary), then it's assigned a 1
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>%
    ungroup() %>% # Ungroup the data
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the tube nest entrance function for the Colony5TubeAggn data set
DistanceCoordsFunctionTrackerTubeAggn5(Colony5TubeAggn)

# BASELINE
DistanceCoordsFunctionTrackerTubeBase5 <- function(data.table){
  TubeCoords <- AggnStudyRefCoords %>% # Reference data set for eight even area bins
    filter(Colony == "5" & Trial == "Pre" & Nest == "Tube") %>% # Filter out only tube nest baseline assay references
    dplyr::select(c(Coord, XREF, YREF)) # Select only desired columns 
  
  # X axis references
  XCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(YREF)) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF1 = as.numeric(XCoords[,1]) # X axis reference from column 1 from above
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  XREF3 = as.numeric(XCoords[,3]) # X axis reference from column 3 from above
  XREF4 = as.numeric(XCoords[,4]) # X axis reference from column 4 from above
  XREF5 = as.numeric(XCoords[,5]) # X axis reference from column 5 from above
  XREF6 = as.numeric(XCoords[,6]) # X axis reference from column 6 from above
  XREF7 = as.numeric(XCoords[,7]) # X axis reference from column 7 from above
  
  # Y axis references
  YCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(XREF)) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # Y axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # Y axis reference from column 2 from above
  YREF3 = as.numeric(YCoords[,3]) # Y axis reference from column 3 from above
  YREF4 = as.numeric(YCoords[,4]) # Y axis reference from column 4 from above
  YREF5 = as.numeric(YCoords[,5]) # Y axis reference from column 5 from above
  YREF6 = as.numeric(YCoords[,6]) # Y axis reference from column 6 from above
  YREF7 = as.numeric(YCoords[,7]) # Y axis reference from column 7 from above
  
  # Width of the tube nest
  Width = YREF2 - YREF3
  
  # Reference distances within the tube nest
  Dist1 = sqrt(((XREF1 - XREF2)^2) + ((YREF1 - YREF2)^2)) # Distance to the entrance from corner entering bin 1 from bin 2
  Dist2 = sqrt(((XREF2 - XREF3)^2) + ((YREF2 - YREF3)^2)) + Dist1 # Distance to the entrance from bin 3 entering bin 2
  Dist3 = sqrt(((XREF4 - XREF3)^2) + ((YREF3 - YREF4)^2)) + Dist2 # Distance to the entrance from bin 4, 5, 6 entering bin 3
  Dist4 = sqrt(((XREF5 - XREF4)^2) + ((YREF5 - YREF4)^2)) + Dist3 # Distance to the entrance from bin 7 entering bin 6
  Dist5 = sqrt(((XREF6 - XREF5)^2) + ((YREF6 - YREF5)^2)) + Dist4 # Distance to the entrance from bin 8 entering bin 7
  MaxDist = sqrt(((XREF6 - XREF7)^2) + ((YREF6 - YREF7)^2)) + Dist5 # Distance from the back of the nest to the entrance
  
  Colony5TubePreDist <<- data.table %>% # Data table to run through the function
    filter(Nest == "Tube") %>% # Filter out tube nest coordinates
    # Coordinate bin assignment
    mutate(X1 = ifelse(X >= XREF2 & X <= (XREF2 + (1.25 * Width)) & Y >= YREF3, # If the x axiss of each coordinate is in bin 1
                       "YES", "NO"), 
           X2 = ifelse(X < XREF2 & Y >= YREF3, # If the x axis of each coordinate is in bin 2
                       "YES", "NO"),
           X3 = ifelse(X <= XREF4 & Y < YREF3, # If the x axis of each coordinate is in bin 3
                       "YES", "NO"),
           X4 = ifelse(X > XREF4 & Y <= YREF5, # If the x axis of each coordinate is in bins 4, 5, or 6
                       "YES", "NO"),
           X5 = ifelse(X >= XREF6 & Y > YREF5, # If the x axis of each coordinate is in bin 7
                       "YES", "NO"),
           X6 = ifelse(X >= XREF7 & Y >= YREF7, # If the x axis of each coordinate is in bin 8
                       "YES", "NO"),
           Y1 = ifelse(X1 == "YES", "YES", "NO"), # If the coordinate qualified under X1
           Y2 = ifelse(X2 == "YES", "YES", "NO"), # If the coordinate qualified under X2
           Y3 = ifelse(X3 == "YES", "YES", "NO"), # If the coordinate qualified under X3
           Y4 = ifelse(X4 == "YES", "YES", "NO"), # If the coordinate qualified under X4
           Y5 = ifelse(X5 == "YES", "YES", "NO"), # If the coordinate qualified under X5
           Y6 = ifelse(X6 == "YES", "YES", "NO"), # If the coordinate qualified under X6
           # X axis distances
           DistanceX = ifelse(X1 == "YES", # Bin 1
                              X - XREF1,
                              ifelse(X2 == "YES", # Bin 2
                                     XREF2 - X,
                                     ifelse(X3 == "YES", # Bin 3
                                            XREF3 - X,
                                            ifelse(X4 == "YES", # Bins 4-6
                                                   X - XREF4,
                                                   ifelse(X5 == "YES", # Bin 7
                                                          X - XREF5,
                                                          ifelse(X6 == "YES", # Bin 8
                                                                 XREF6 - X,
                                                                 NA)))))), # Else NA
           # Y axis distances
           DistanceY = ifelse(Y1 == "YES", # Bin 1
                              YREF1 - Y,
                              ifelse(Y2 == "YES", # Bin 2
                                     YREF2 - Y,
                                     ifelse(Y3 == "YES" | Y4 == "YES" & X < XREF4, # Bin 3, or bin 4 with x coordinates less than the bin's x axis reference
                                            YREF3 - Y,
                                            ifelse(Y4 == "YES", # Bin 4-6
                                                   YREF4 - Y,
                                                   ifelse(Y5 == "YES", # Bin 7
                                                          Y - YREF5,
                                                          ifelse(Y6 == "YES", # Bin 8
                                                                 YREF6 - Y,
                                                                 NA)))))),
           # Shortest distances from the nest entrance
           # Only bin 1 is a direct line to the entrance, every other bin is the shortest distance to the closest bin to the entrance + reference distance for that bin to the entrance
           PythagDist = ifelse(X1 == "YES" & Y1 == "YES", # Bin 1
                               sqrt((DistanceX^2) + (DistanceY^2)),
                               ifelse(X2 == "YES" & Y2 == "YES", # Bin 2
                                      sqrt((DistanceX^2) + (DistanceY^2)) + Dist1,
                                      ifelse(X3 == "YES" & Y3 == "YES", # Bin 3
                                             sqrt((DistanceX^2) + (DistanceY^2)) + Dist2,
                                             ifelse(X4 == "YES" & Y4 == "YES", # Bin 4-6
                                                    sqrt((DistanceX^2) + (DistanceY^2)) + Dist3, 
                                                    ifelse(X5 == "YES" & Y5 == "YES", # Bin 7
                                                           sqrt((DistanceX^2) + (DistanceY^2)) + Dist4,
                                                           ifelse(X6 == "YES" & Y6 == "YES", # Bin 8
                                                                  sqrt((DistanceX^2) + (DistanceY^2)) + Dist5,
                                                                  NA)))))),
           # Scaling distances by the max possible distance
           ScaledDist = PythagDist / MaxDist,
           # If any distance is greater than 1 (precautionary), then it's assigned a 1
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>%
    ungroup() %>% # Ungroup the data
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the tube nest entrance function for the Colony5TubePre data set
DistanceCoordsFunctionTrackerTubeBase5(Colony5TubePre)

# CIRCLE NEST
# INVADER
DistanceCoordsFunctionTrackerCircleAggn5 <-function(data.table){
  CircleCoords <- AggnStudyCircleRefCoords %>% # Reference coordinates for the circle nest
    filter(Colony == "5" & Trial == "Aggn" & Nest == "Circle") %>% # Filtering out the aggression assay references
    dplyr::select(c("Coord", "XREF", "YREF")) # Select only the desired columns
  
  # X axis references
  XCoords <- CircleCoords %>% # Creating a x axis reference data set
    dplyr::select(-c("YREF")) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  
  # Y axis references
  YCoords <- CircleCoords %>% # Creating a y axis reference data set
    dplyr::select(-c("XREF")) # Remove x axis references
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # X axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # X axis reference from column 2 from above
  MaxDist = abs(YREF1 - YREF2) # Distance from the back of the nest to the entrance
  
  Colony5CircleAggnDist <<- data.table %>% # Data table to run through the function
    mutate(DistanceX = XREF2 - X, # X axis distance to the entrance
           DistanceY = max(Y) - Y, # Y axis distance to the entrance
           PythagDist = sqrt((DistanceX^2) + (DistanceY^2)), # Shortest distance to the nest entrance 
           ScaledDist = PythagDist / MaxDist, # Scaling the distances by the max possible distance
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>% # If any distance is greater than 1 (precautionary), then it's assigned a 1
    ungroup() %>% # Ungroup the data set
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the circle nest entrance function for the Colony5CircleAggn data set
DistanceCoordsFunctionTrackerCircleAggn5(Colony5CircleAggn)

# BASELINE
DistanceCoordsFunctionTrackerCircleBase5 <- function(data.table){
  CircleCoords <- AggnStudyCircleRefCoords %>% # Reference coordinates for the circle nest
    filter(Colony == "5" & Trial == "Pre" & Nest == "Circle") %>% # Filtering out the baseline assay references
    dplyr::select(c("Coord", "XREF", "YREF")) # Select only the desired columns
  
  # X axis references
  XCoords <- CircleCoords %>% # Creating a x axis reference data set
    dplyr::select(-c("YREF")) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  
  # Y axis references
  YCoords <- CircleCoords %>% # Creating a y axis reference data set
    dplyr::select(-c("XREF")) # Remove x axis references
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # X axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # X axis reference from column 2 from above
  MaxDist = abs(YREF1 - YREF2) # Distance from the back of the nest to the entrance
  
  Colony5CirclePreDist <<- data.table %>% # Data table to run through the function
    mutate(DistanceX = XREF2 - X, # X axis distance to the entrance
           DistanceY = max(Y) - Y, # Y axis distance to the entrance
           PythagDist = sqrt((DistanceX^2) + (DistanceY^2)), # Shortest distance to the nest entrance 
           ScaledDist = PythagDist / MaxDist, # Scaling the distances by the max possible distance
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>% # If any distance is greater than 1 (precautionary), then it's assigned a 1
    ungroup() %>% # Ungroup the data set
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the circle nest entrance function for the Colony5CirclePre data set
DistanceCoordsFunctionTrackerCircleBase5(Colony5CirclePre)

# COLONY 6
# TUBE NEST
# INVADER
DistanceCoordsFunctionTrackerTubeAggn6 <- function(data.table){
  TubeCoords <- AggnStudyRefCoords %>% # Reference data set for eight even area bins
    filter(Colony == "6" & Trial == "Aggn" & Nest == "Tube") %>% # Filter out only tube nest aggression assay references
    dplyr::select(c(Coord, XREF, YREF)) # Select only desired columns 
  
  # X axis references
  XCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(YREF)) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF1 = as.numeric(XCoords[,1]) # X axis reference from column 1 from above
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  XREF3 = as.numeric(XCoords[,3]) # X axis reference from column 3 from above
  XREF4 = as.numeric(XCoords[,4]) # X axis reference from column 4 from above
  XREF5 = as.numeric(XCoords[,5]) # X axis reference from column 5 from above
  XREF6 = as.numeric(XCoords[,6]) # X axis reference from column 6 from above
  XREF7 = as.numeric(XCoords[,7]) # X axis reference from column 7 from above
  
  # Y axis references
  YCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(XREF)) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # Y axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # Y axis reference from column 2 from above
  YREF3 = as.numeric(YCoords[,3]) # Y axis reference from column 3 from above
  YREF4 = as.numeric(YCoords[,4]) # Y axis reference from column 4 from above
  YREF5 = as.numeric(YCoords[,5]) # Y axis reference from column 5 from above
  YREF6 = as.numeric(YCoords[,6]) # Y axis reference from column 6 from above
  YREF7 = as.numeric(YCoords[,7]) # Y axis reference from column 7 from above
  
  # Width of the tube nest
  Width = YREF2 - YREF3
  
  # Reference distances within the tube nest
  Dist1 = sqrt(((XREF1 - XREF2)^2) + ((YREF1 - YREF2)^2)) # Distance to the entrance from corner entering bin 1 from bin 2
  Dist2 = sqrt(((XREF2 - XREF3)^2) + ((YREF2 - YREF3)^2)) + Dist1 # Distance to the entrance from bin 3 entering bin 2
  Dist3 = sqrt(((XREF4 - XREF3)^2) + ((YREF3 - YREF4)^2)) + Dist2 # Distance to the entrance from bin 4, 5, 6 entering bin 3
  Dist4 = sqrt(((XREF5 - XREF4)^2) + ((YREF5 - YREF4)^2)) + Dist3 # Distance to the entrance from bin 7 entering bin 6
  Dist5 = sqrt(((XREF6 - XREF5)^2) + ((YREF6 - YREF5)^2)) + Dist4 # Distance to the entrance from bin 8 entering bin 7
  MaxDist = sqrt(((XREF6 - XREF7)^2) + ((YREF6 - YREF7)^2)) + Dist5 # Distance from the back of the nest to the entrance
  
  Colony6TubeAggnDist <<- data.table %>% # Data table to run through the function
    filter(Nest == "Tube") %>% # Filter out tube nest coordinates
    # Coordinate bin assignment
    mutate(X1 = ifelse(X >= XREF2 & X <= (XREF2 + (1.25 * Width)) & Y >= YREF3, # If the x axiss of each coordinate is in bin 1
                       "YES", "NO"), 
           X2 = ifelse(X < XREF2 & Y >= YREF3, # If the x axis of each coordinate is in bin 2
                       "YES", "NO"),
           X3 = ifelse(X <= XREF4 & Y < YREF3, # If the x axis of each coordinate is in bin 3
                       "YES", "NO"),
           X4 = ifelse(X > XREF4 & Y <= YREF5, # If the x axis of each coordinate is in bins 4, 5, or 6
                       "YES", "NO"),
           X5 = ifelse(X >= XREF6 & Y > YREF5, # If the x axis of each coordinate is in bin 7
                       "YES", "NO"),
           X6 = ifelse(X >= XREF7 & Y >= YREF7, # If the x axis of each coordinate is in bin 8
                       "YES", "NO"),
           Y1 = ifelse(X1 == "YES", "YES", "NO"), # If the coordinate qualified under X1
           Y2 = ifelse(X2 == "YES", "YES", "NO"), # If the coordinate qualified under X2
           Y3 = ifelse(X3 == "YES", "YES", "NO"), # If the coordinate qualified under X3
           Y4 = ifelse(X4 == "YES", "YES", "NO"), # If the coordinate qualified under X4
           Y5 = ifelse(X5 == "YES", "YES", "NO"), # If the coordinate qualified under X5
           Y6 = ifelse(X6 == "YES", "YES", "NO"), # If the coordinate qualified under X6
           # X axis distances
           DistanceX = ifelse(X1 == "YES", # Bin 1
                              X - XREF1,
                              ifelse(X2 == "YES", # Bin 2
                                     XREF2 - X,
                                     ifelse(X3 == "YES", # Bin 3
                                            XREF3 - X,
                                            ifelse(X4 == "YES", # Bins 4-6
                                                   X - XREF4,
                                                   ifelse(X5 == "YES", # Bin 7
                                                          X - XREF5,
                                                          ifelse(X6 == "YES", # Bin 8
                                                                 XREF6 - X,
                                                                 NA)))))), # Else NA
           # Y axis distances
           DistanceY = ifelse(Y1 == "YES", # Bin 1
                              YREF1 - Y,
                              ifelse(Y2 == "YES", # Bin 2
                                     YREF2 - Y,
                                     ifelse(Y3 == "YES" | Y4 == "YES" & X < XREF4, # Bin 3, or bin 4 with x coordinates less than the bin's x axis reference
                                            YREF3 - Y,
                                            ifelse(Y4 == "YES", # Bin 4-6
                                                   YREF4 - Y,
                                                   ifelse(Y5 == "YES", # Bin 7
                                                          Y - YREF5,
                                                          ifelse(Y6 == "YES", # Bin 8
                                                                 YREF6 - Y,
                                                                 NA)))))),
           # Shortest distances from the nest entrance
           # Only bin 1 is a direct line to the entrance, every other bin is the shortest distance to the closest bin to the entrance + reference distance for that bin to the entrance
           PythagDist = ifelse(X1 == "YES" & Y1 == "YES", # Bin 1
                               sqrt((DistanceX^2) + (DistanceY^2)),
                               ifelse(X2 == "YES" & Y2 == "YES", # Bin 2
                                      sqrt((DistanceX^2) + (DistanceY^2)) + Dist1,
                                      ifelse(X3 == "YES" & Y3 == "YES", # Bin 3
                                             sqrt((DistanceX^2) + (DistanceY^2)) + Dist2,
                                             ifelse(X4 == "YES" & Y4 == "YES", # Bin 4-6
                                                    sqrt((DistanceX^2) + (DistanceY^2)) + Dist3, 
                                                    ifelse(X5 == "YES" & Y5 == "YES", # Bin 7
                                                           sqrt((DistanceX^2) + (DistanceY^2)) + Dist4,
                                                           ifelse(X6 == "YES" & Y6 == "YES", # Bin 8
                                                                  sqrt((DistanceX^2) + (DistanceY^2)) + Dist5,
                                                                  NA)))))),
           # Scaling distances by the max possible distance
           ScaledDist = PythagDist / MaxDist,
           # If any distance is greater than 1 (precautionary), then it's assigned a 1
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>%
    ungroup() %>% # Ungroup the data
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the tube nest entrance function for the Colony6TubeAggn data set
DistanceCoordsFunctionTrackerTubeAggn6(Colony6TubeAggn)

# BASELINE
DistanceCoordsFunctionTrackerTubeBase6 <- function(data.table){
  TubeCoords <- AggnStudyRefCoords %>% # Reference data set for eight even area bins
    filter(Colony == "6" & Trial == "Pre" & Nest == "Tube") %>% # Filter out only tube nest baseline assay references
    dplyr::select(c(Coord, XREF, YREF)) # Select only desired columns 
  
  # X axis references
  XCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(YREF)) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF1 = as.numeric(XCoords[,1]) # X axis reference from column 1 from above
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  XREF3 = as.numeric(XCoords[,3]) # X axis reference from column 3 from above
  XREF4 = as.numeric(XCoords[,4]) # X axis reference from column 4 from above
  XREF5 = as.numeric(XCoords[,5]) # X axis reference from column 5 from above
  XREF6 = as.numeric(XCoords[,6]) # X axis reference from column 6 from above
  XREF7 = as.numeric(XCoords[,7]) # X axis reference from column 7 from above
  
  # Y axis references
  YCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(XREF)) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # Y axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # Y axis reference from column 2 from above
  YREF3 = as.numeric(YCoords[,3]) # Y axis reference from column 3 from above
  YREF4 = as.numeric(YCoords[,4]) # Y axis reference from column 4 from above
  YREF5 = as.numeric(YCoords[,5]) # Y axis reference from column 5 from above
  YREF6 = as.numeric(YCoords[,6]) # Y axis reference from column 6 from above
  YREF7 = as.numeric(YCoords[,7]) # Y axis reference from column 7 from above
  
  # Width of the tube nest
  Width = YREF2 - YREF3
  
  # Reference distances within the tube nest
  Dist1 = sqrt(((XREF1 - XREF2)^2) + ((YREF1 - YREF2)^2)) # Distance to the entrance from corner entering bin 1 from bin 2
  Dist2 = sqrt(((XREF2 - XREF3)^2) + ((YREF2 - YREF3)^2)) + Dist1 # Distance to the entrance from bin 3 entering bin 2
  Dist3 = sqrt(((XREF4 - XREF3)^2) + ((YREF3 - YREF4)^2)) + Dist2 # Distance to the entrance from bin 4, 5, 6 entering bin 3
  Dist4 = sqrt(((XREF5 - XREF4)^2) + ((YREF5 - YREF4)^2)) + Dist3 # Distance to the entrance from bin 7 entering bin 6
  Dist5 = sqrt(((XREF6 - XREF5)^2) + ((YREF6 - YREF5)^2)) + Dist4 # Distance to the entrance from bin 8 entering bin 7
  MaxDist = sqrt(((XREF6 - XREF7)^2) + ((YREF6 - YREF7)^2)) + Dist5 # Distance from the back of the nest to the entrance
  
  Colony6TubePreDist <<- data.table %>% # Data table to run through the function
    filter(Nest == "Tube") %>% # Filter out tube nest coordinates
    # Coordinate bin assignment
    mutate(X1 = ifelse(X >= XREF2 & X <= (XREF2 + (1.25 * Width)) & Y >= YREF3, # If the x axiss of each coordinate is in bin 1
                       "YES", "NO"), 
           X2 = ifelse(X < XREF2 & Y >= YREF3, # If the x axis of each coordinate is in bin 2
                       "YES", "NO"),
           X3 = ifelse(X <= XREF4 & Y < YREF3, # If the x axis of each coordinate is in bin 3
                       "YES", "NO"),
           X4 = ifelse(X > XREF4 & Y <= YREF5, # If the x axis of each coordinate is in bins 4, 5, or 6
                       "YES", "NO"),
           X5 = ifelse(X >= XREF6 & Y > YREF5, # If the x axis of each coordinate is in bin 7
                       "YES", "NO"),
           X6 = ifelse(X >= XREF7 & Y >= YREF7, # If the x axis of each coordinate is in bin 8
                       "YES", "NO"),
           Y1 = ifelse(X1 == "YES", "YES", "NO"), # If the coordinate qualified under X1
           Y2 = ifelse(X2 == "YES", "YES", "NO"), # If the coordinate qualified under X2
           Y3 = ifelse(X3 == "YES", "YES", "NO"), # If the coordinate qualified under X3
           Y4 = ifelse(X4 == "YES", "YES", "NO"), # If the coordinate qualified under X4
           Y5 = ifelse(X5 == "YES", "YES", "NO"), # If the coordinate qualified under X5
           Y6 = ifelse(X6 == "YES", "YES", "NO"), # If the coordinate qualified under X6
           # X axis distances
           DistanceX = ifelse(X1 == "YES", # Bin 1
                              X - XREF1,
                              ifelse(X2 == "YES", # Bin 2
                                     XREF2 - X,
                                     ifelse(X3 == "YES", # Bin 3
                                            XREF3 - X,
                                            ifelse(X4 == "YES", # Bins 4-6
                                                   X - XREF4,
                                                   ifelse(X5 == "YES", # Bin 7
                                                          X - XREF5,
                                                          ifelse(X6 == "YES", # Bin 8
                                                                 XREF6 - X,
                                                                 NA)))))), # Else NA
           # Y axis distances
           DistanceY = ifelse(Y1 == "YES", # Bin 1
                              YREF1 - Y,
                              ifelse(Y2 == "YES", # Bin 2
                                     YREF2 - Y,
                                     ifelse(Y3 == "YES" | Y4 == "YES" & X < XREF4, # Bin 3, or bin 4 with x coordinates less than the bin's x axis reference
                                            YREF3 - Y,
                                            ifelse(Y4 == "YES", # Bin 4-6
                                                   YREF4 - Y,
                                                   ifelse(Y5 == "YES", # Bin 7
                                                          Y - YREF5,
                                                          ifelse(Y6 == "YES", # Bin 8
                                                                 YREF6 - Y,
                                                                 NA)))))),
           # Shortest distances from the nest entrance
           # Only bin 1 is a direct line to the entrance, every other bin is the shortest distance to the closest bin to the entrance + reference distance for that bin to the entrance
           PythagDist = ifelse(X1 == "YES" & Y1 == "YES", # Bin 1
                               sqrt((DistanceX^2) + (DistanceY^2)),
                               ifelse(X2 == "YES" & Y2 == "YES", # Bin 2
                                      sqrt((DistanceX^2) + (DistanceY^2)) + Dist1,
                                      ifelse(X3 == "YES" & Y3 == "YES", # Bin 3
                                             sqrt((DistanceX^2) + (DistanceY^2)) + Dist2,
                                             ifelse(X4 == "YES" & Y4 == "YES", # Bin 4-6
                                                    sqrt((DistanceX^2) + (DistanceY^2)) + Dist3, 
                                                    ifelse(X5 == "YES" & Y5 == "YES", # Bin 7
                                                           sqrt((DistanceX^2) + (DistanceY^2)) + Dist4,
                                                           ifelse(X6 == "YES" & Y6 == "YES", # Bin 8
                                                                  sqrt((DistanceX^2) + (DistanceY^2)) + Dist5,
                                                                  NA)))))),
           # Scaling distances by the max possible distance
           ScaledDist = PythagDist / MaxDist,
           # If any distance is greater than 1 (precautionary), then it's assigned a 1
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>%
    ungroup() %>% # Ungroup the data
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the tube nest entrance function for the Colony6TubePre data set
DistanceCoordsFunctionTrackerTubeBase6(Colony6TubePre)

# CIRCLE NEST
# INVADER
DistanceCoordsFunctionTrackerCircleAggn6 <-function(data.table){
  CircleCoords <- AggnStudyCircleRefCoords %>% # Reference coordinates for the circle nest
    filter(Colony == "6" & Trial == "Aggn" & Nest == "Circle") %>% # Filtering out the aggression assay references
    dplyr::select(c("Coord", "XREF", "YREF")) # Select only the desired columns
  
  # X axis references
  XCoords <- CircleCoords %>% # Creating a x axis reference data set
    dplyr::select(-c("YREF")) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  
  # Y axis references
  YCoords <- CircleCoords %>% # Creating a y axis reference data set
    dplyr::select(-c("XREF")) # Remove x axis references
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # X axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # X axis reference from column 2 from above
  MaxDist = abs(YREF1 - YREF2) # Distance from the back of the nest to the entrance
  
  Colony6CircleAggnDist <<- data.table %>% # Data table to run through the function
    mutate(DistanceX = XREF2 - X, # X axis distance to the entrance
           DistanceY = max(Y) - Y, # Y axis distance to the entrance
           PythagDist = sqrt((DistanceX^2) + (DistanceY^2)), # Shortest distance to the nest entrance 
           ScaledDist = PythagDist / MaxDist, # Scaling the distances by the max possible distance
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>% # If any distance is greater than 1 (precautionary), then it's assigned a 1
    ungroup() %>% # Ungroup the data set
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the circle nest entrance function for the Colony6CircleAggn data set
DistanceCoordsFunctionTrackerCircleAggn6(Colony6CircleAggn)

# BASELINE
DistanceCoordsFunctionTrackerCircleBase6 <- function(data.table){
  CircleCoords <- AggnStudyCircleRefCoords %>% # Reference coordinates for the circle nest
    filter(Colony == "6" & Trial == "Pre" & Nest == "Circle") %>% # Filtering out the baseline assay references
    dplyr::select(c("Coord", "XREF", "YREF")) # Select only the desired columns
  
  # X axis references
  XCoords <- CircleCoords %>% # Creating a x axis reference data set
    dplyr::select(-c("YREF")) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  
  # Y axis references
  YCoords <- CircleCoords %>% # Creating a y axis reference data set
    dplyr::select(-c("XREF")) # Remove x axis references
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # X axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # X axis reference from column 2 from above
  MaxDist = abs(YREF1 - YREF2) # Distance from the back of the nest to the entrance
  
  Colony6CirclePreDist <<- data.table %>% # Data table to run through the function
    mutate(DistanceX = XREF2 - X, # X axis distance to the entrance
           DistanceY = max(Y) - Y, # Y axis distance to the entrance
           PythagDist = sqrt((DistanceX^2) + (DistanceY^2)), # Shortest distance to the nest entrance 
           ScaledDist = PythagDist / MaxDist, # Scaling the distances by the max possible distance
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>% # If any distance is greater than 1 (precautionary), then it's assigned a 1
    ungroup() %>% # Ungroup the data set
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the circle nest entrance function for the Colony6CirclePre data set
DistanceCoordsFunctionTrackerCircleBase6(Colony6CirclePre)

# COLONY 7
# TUBE NEST
# INVADER
DistanceCoordsFunctionTrackerTubeAggn7 <- function(data.table){
  TubeCoords <- AggnStudyRefCoords %>% # Reference data set for eight even area bins
    filter(Colony == "7" & Trial == "Aggn" & Nest == "Tube") %>% # Filter out only tube nest aggression assay references
    dplyr::select(c(Coord, XREF, YREF)) # Select only desired columns 
  
  # X axis references
  XCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(YREF)) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF1 = as.numeric(XCoords[,1]) # X axis reference from column 1 from above
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  XREF3 = as.numeric(XCoords[,3]) # X axis reference from column 3 from above
  XREF4 = as.numeric(XCoords[,4]) # X axis reference from column 4 from above
  XREF5 = as.numeric(XCoords[,5]) # X axis reference from column 5 from above
  XREF6 = as.numeric(XCoords[,6]) # X axis reference from column 6 from above
  XREF7 = as.numeric(XCoords[,7]) # X axis reference from column 7 from above
  
  # Y axis references
  YCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(XREF)) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # Y axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # Y axis reference from column 2 from above
  YREF3 = as.numeric(YCoords[,3]) # Y axis reference from column 3 from above
  YREF4 = as.numeric(YCoords[,4]) # Y axis reference from column 4 from above
  YREF5 = as.numeric(YCoords[,5]) # Y axis reference from column 5 from above
  YREF6 = as.numeric(YCoords[,6]) # Y axis reference from column 6 from above
  YREF7 = as.numeric(YCoords[,7]) # Y axis reference from column 7 from above
  
  # Width of the tube nest
  Width = YREF2 - YREF3
  
  # Reference distances within the tube nest
  Dist1 = sqrt(((XREF1 - XREF2)^2) + ((YREF1 - YREF2)^2)) # Distance to the entrance from corner entering bin 1 from bin 2
  Dist2 = sqrt(((XREF2 - XREF3)^2) + ((YREF2 - YREF3)^2)) + Dist1 # Distance to the entrance from bin 3 entering bin 2
  Dist3 = sqrt(((XREF4 - XREF3)^2) + ((YREF3 - YREF4)^2)) + Dist2 # Distance to the entrance from bin 4, 5, 6 entering bin 3
  Dist4 = sqrt(((XREF5 - XREF4)^2) + ((YREF5 - YREF4)^2)) + Dist3 # Distance to the entrance from bin 7 entering bin 6
  Dist5 = sqrt(((XREF6 - XREF5)^2) + ((YREF6 - YREF5)^2)) + Dist4 # Distance to the entrance from bin 8 entering bin 7
  MaxDist = sqrt(((XREF6 - XREF7)^2) + ((YREF6 - YREF7)^2)) + Dist5 # Distance from the back of the nest to the entrance
  
  Colony7TubeAggnDist <<- data.table %>% # Data table to run through the function
    filter(Nest == "Tube") %>% # Filter out tube nest coordinates
    # Coordinate bin assignment
    mutate(X1 = ifelse(X >= XREF2 & X <= (XREF2 + (1.25 * Width)) & Y >= YREF3, # If the x axiss of each coordinate is in bin 1
                       "YES", "NO"), 
           X2 = ifelse(X < XREF2 & Y >= YREF3, # If the x axis of each coordinate is in bin 2
                       "YES", "NO"),
           X3 = ifelse(X <= XREF4 & Y < YREF3, # If the x axis of each coordinate is in bin 3
                       "YES", "NO"),
           X4 = ifelse(X > XREF4 & Y <= YREF5, # If the x axis of each coordinate is in bins 4, 5, or 6
                       "YES", "NO"),
           X5 = ifelse(X >= XREF6 & Y > YREF5, # If the x axis of each coordinate is in bin 7
                       "YES", "NO"),
           X6 = ifelse(X >= XREF7 & Y >= YREF7, # If the x axis of each coordinate is in bin 8
                       "YES", "NO"),
           Y1 = ifelse(X1 == "YES", "YES", "NO"), # If the coordinate qualified under X1
           Y2 = ifelse(X2 == "YES", "YES", "NO"), # If the coordinate qualified under X2
           Y3 = ifelse(X3 == "YES", "YES", "NO"), # If the coordinate qualified under X3
           Y4 = ifelse(X4 == "YES", "YES", "NO"), # If the coordinate qualified under X4
           Y5 = ifelse(X5 == "YES", "YES", "NO"), # If the coordinate qualified under X5
           Y6 = ifelse(X6 == "YES", "YES", "NO"), # If the coordinate qualified under X6
           # X axis distances
           DistanceX = ifelse(X1 == "YES", # Bin 1
                              X - XREF1,
                              ifelse(X2 == "YES", # Bin 2
                                     XREF2 - X,
                                     ifelse(X3 == "YES", # Bin 3
                                            XREF3 - X,
                                            ifelse(X4 == "YES", # Bins 4-6
                                                   X - XREF4,
                                                   ifelse(X5 == "YES", # Bin 7
                                                          X - XREF5,
                                                          ifelse(X6 == "YES", # Bin 8
                                                                 XREF6 - X,
                                                                 NA)))))), # Else NA
           # Y axis distances
           DistanceY = ifelse(Y1 == "YES", # Bin 1
                              YREF1 - Y,
                              ifelse(Y2 == "YES", # Bin 2
                                     YREF2 - Y,
                                     ifelse(Y3 == "YES" | Y4 == "YES" & X < XREF4, # Bin 3, or bin 4 with x coordinates less than the bin's x axis reference
                                            YREF3 - Y,
                                            ifelse(Y4 == "YES", # Bin 4-6
                                                   YREF4 - Y,
                                                   ifelse(Y5 == "YES", # Bin 7
                                                          Y - YREF5,
                                                          ifelse(Y6 == "YES", # Bin 8
                                                                 YREF6 - Y,
                                                                 NA)))))),
           # Shortest distances from the nest entrance
           # Only bin 1 is a direct line to the entrance, every other bin is the shortest distance to the closest bin to the entrance + reference distance for that bin to the entrance
           PythagDist = ifelse(X1 == "YES" & Y1 == "YES", # Bin 1
                               sqrt((DistanceX^2) + (DistanceY^2)),
                               ifelse(X2 == "YES" & Y2 == "YES", # Bin 2
                                      sqrt((DistanceX^2) + (DistanceY^2)) + Dist1,
                                      ifelse(X3 == "YES" & Y3 == "YES", # Bin 3
                                             sqrt((DistanceX^2) + (DistanceY^2)) + Dist2,
                                             ifelse(X4 == "YES" & Y4 == "YES", # Bin 4-6
                                                    sqrt((DistanceX^2) + (DistanceY^2)) + Dist3, 
                                                    ifelse(X5 == "YES" & Y5 == "YES", # Bin 7
                                                           sqrt((DistanceX^2) + (DistanceY^2)) + Dist4,
                                                           ifelse(X6 == "YES" & Y6 == "YES", # Bin 8
                                                                  sqrt((DistanceX^2) + (DistanceY^2)) + Dist5,
                                                                  NA)))))),
           # Scaling distances by the max possible distance
           ScaledDist = PythagDist / MaxDist,
           # If any distance is greater than 1 (precautionary), then it's assigned a 1
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>%
    ungroup() %>% # Ungroup the data
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the tube nest entrance function for the Colony7TubeAggn data set
DistanceCoordsFunctionTrackerTubeAggn7(Colony7TubeAggn)

# BASELINE
DistanceCoordsFunctionTrackerTubeBase7 <- function(data.table){
  TubeCoords <- AggnStudyRefCoords %>% # Reference data set for eight even area bins
    filter(Colony == "7" & Trial == "Pre" & Nest == "Tube") %>% # Filter out only tube nest baseline assay references
    dplyr::select(c(Coord, XREF, YREF)) # Select only desired columns 
  
  # X axis references
  XCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(YREF)) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF1 = as.numeric(XCoords[,1]) # X axis reference from column 1 from above
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  XREF3 = as.numeric(XCoords[,3]) # X axis reference from column 3 from above
  XREF4 = as.numeric(XCoords[,4]) # X axis reference from column 4 from above
  XREF5 = as.numeric(XCoords[,5]) # X axis reference from column 5 from above
  XREF6 = as.numeric(XCoords[,6]) # X axis reference from column 6 from above
  XREF7 = as.numeric(XCoords[,7]) # X axis reference from column 7 from above
  
  # Y axis references
  YCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(XREF)) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # Y axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # Y axis reference from column 2 from above
  YREF3 = as.numeric(YCoords[,3]) # Y axis reference from column 3 from above
  YREF4 = as.numeric(YCoords[,4]) # Y axis reference from column 4 from above
  YREF5 = as.numeric(YCoords[,5]) # Y axis reference from column 5 from above
  YREF6 = as.numeric(YCoords[,6]) # Y axis reference from column 6 from above
  YREF7 = as.numeric(YCoords[,7]) # Y axis reference from column 7 from above
  
  # Width of the tube nest
  Width = YREF2 - YREF3
  
  # Reference distances within the tube nest
  Dist1 = sqrt(((XREF1 - XREF2)^2) + ((YREF1 - YREF2)^2)) # Distance to the entrance from corner entering bin 1 from bin 2
  Dist2 = sqrt(((XREF2 - XREF3)^2) + ((YREF2 - YREF3)^2)) + Dist1 # Distance to the entrance from bin 3 entering bin 2
  Dist3 = sqrt(((XREF4 - XREF3)^2) + ((YREF3 - YREF4)^2)) + Dist2 # Distance to the entrance from bin 4, 5, 6 entering bin 3
  Dist4 = sqrt(((XREF5 - XREF4)^2) + ((YREF5 - YREF4)^2)) + Dist3 # Distance to the entrance from bin 7 entering bin 6
  Dist5 = sqrt(((XREF6 - XREF5)^2) + ((YREF6 - YREF5)^2)) + Dist4 # Distance to the entrance from bin 8 entering bin 7
  MaxDist = sqrt(((XREF6 - XREF7)^2) + ((YREF6 - YREF7)^2)) + Dist5 # Distance from the back of the nest to the entrance
  
  Colony7TubePreDist <<- data.table %>% # Data table to run through the function
    filter(Nest == "Tube") %>% # Filter out tube nest coordinates
    # Coordinate bin assignment
    mutate(X1 = ifelse(X >= XREF2 & X <= (XREF2 + (1.25 * Width)) & Y >= YREF3, # If the x axiss of each coordinate is in bin 1
                       "YES", "NO"), 
           X2 = ifelse(X < XREF2 & Y >= YREF3, # If the x axis of each coordinate is in bin 2
                       "YES", "NO"),
           X3 = ifelse(X <= XREF4 & Y < YREF3, # If the x axis of each coordinate is in bin 3
                       "YES", "NO"),
           X4 = ifelse(X > XREF4 & Y <= YREF5, # If the x axis of each coordinate is in bins 4, 5, or 6
                       "YES", "NO"),
           X5 = ifelse(X >= XREF6 & Y > YREF5, # If the x axis of each coordinate is in bin 7
                       "YES", "NO"),
           X6 = ifelse(X >= XREF7 & Y >= YREF7, # If the x axis of each coordinate is in bin 8
                       "YES", "NO"),
           Y1 = ifelse(X1 == "YES", "YES", "NO"), # If the coordinate qualified under X1
           Y2 = ifelse(X2 == "YES", "YES", "NO"), # If the coordinate qualified under X2
           Y3 = ifelse(X3 == "YES", "YES", "NO"), # If the coordinate qualified under X3
           Y4 = ifelse(X4 == "YES", "YES", "NO"), # If the coordinate qualified under X4
           Y5 = ifelse(X5 == "YES", "YES", "NO"), # If the coordinate qualified under X5
           Y6 = ifelse(X6 == "YES", "YES", "NO"), # If the coordinate qualified under X6
           # X axis distances
           DistanceX = ifelse(X1 == "YES", # Bin 1
                              X - XREF1,
                              ifelse(X2 == "YES", # Bin 2
                                     XREF2 - X,
                                     ifelse(X3 == "YES", # Bin 3
                                            XREF3 - X,
                                            ifelse(X4 == "YES", # Bins 4-6
                                                   X - XREF4,
                                                   ifelse(X5 == "YES", # Bin 7
                                                          X - XREF5,
                                                          ifelse(X6 == "YES", # Bin 8
                                                                 XREF6 - X,
                                                                 NA)))))), # Else NA
           # Y axis distances
           DistanceY = ifelse(Y1 == "YES", # Bin 1
                              YREF1 - Y,
                              ifelse(Y2 == "YES", # Bin 2
                                     YREF2 - Y,
                                     ifelse(Y3 == "YES" | Y4 == "YES" & X < XREF4, # Bin 3, or bin 4 with x coordinates less than the bin's x axis reference
                                            YREF3 - Y,
                                            ifelse(Y4 == "YES", # Bin 4-6
                                                   YREF4 - Y,
                                                   ifelse(Y5 == "YES", # Bin 7
                                                          Y - YREF5,
                                                          ifelse(Y6 == "YES", # Bin 8
                                                                 YREF6 - Y,
                                                                 NA)))))),
           # Shortest distances from the nest entrance
           # Only bin 1 is a direct line to the entrance, every other bin is the shortest distance to the closest bin to the entrance + reference distance for that bin to the entrance
           PythagDist = ifelse(X1 == "YES" & Y1 == "YES", # Bin 1
                               sqrt((DistanceX^2) + (DistanceY^2)),
                               ifelse(X2 == "YES" & Y2 == "YES", # Bin 2
                                      sqrt((DistanceX^2) + (DistanceY^2)) + Dist1,
                                      ifelse(X3 == "YES" & Y3 == "YES", # Bin 3
                                             sqrt((DistanceX^2) + (DistanceY^2)) + Dist2,
                                             ifelse(X4 == "YES" & Y4 == "YES", # Bin 4-6
                                                    sqrt((DistanceX^2) + (DistanceY^2)) + Dist3, 
                                                    ifelse(X5 == "YES" & Y5 == "YES", # Bin 7
                                                           sqrt((DistanceX^2) + (DistanceY^2)) + Dist4,
                                                           ifelse(X6 == "YES" & Y6 == "YES", # Bin 8
                                                                  sqrt((DistanceX^2) + (DistanceY^2)) + Dist5,
                                                                  NA)))))),
           # Scaling distances by the max possible distance
           ScaledDist = PythagDist / MaxDist,
           # If any distance is greater than 1 (precautionary), then it's assigned a 1
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>%
    ungroup() %>% # Ungroup the data
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the tube nest entrance function for the Colony7TubePre data set
DistanceCoordsFunctionTrackerTubeBase7(Colony7TubePre)

# CIRCLE NEST
# INVADER
DistanceCoordsFunctionTrackerCircleAggn7 <-function(data.table){
  CircleCoords <- AggnStudyCircleRefCoords %>% # Reference coordinates for the circle nest
    filter(Colony == "7" & Trial == "Aggn" & Nest == "Circle") %>% # Filtering out the aggression assay references
    dplyr::select(c("Coord", "XREF", "YREF")) # Select only the desired columns
  
  # X axis references
  XCoords <- CircleCoords %>% # Creating a x axis reference data set
    dplyr::select(-c("YREF")) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  
  # Y axis references
  YCoords <- CircleCoords %>% # Creating a y axis reference data set
    dplyr::select(-c("XREF")) # Remove x axis references
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # X axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # X axis reference from column 2 from above
  MaxDist = abs(YREF1 - YREF2) # Distance from the back of the nest to the entrance
  
  Colony7CircleAggnDist <<- data.table %>% # Data table to run through the function
    mutate(DistanceX = XREF2 - X, # X axis distance to the entrance
           DistanceY = max(Y) - Y, # Y axis distance to the entrance
           PythagDist = sqrt((DistanceX^2) + (DistanceY^2)), # Shortest distance to the nest entrance 
           ScaledDist = PythagDist / MaxDist, # Scaling the distances by the max possible distance
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>% # If any distance is greater than 1 (precautionary), then it's assigned a 1
    ungroup() %>% # Ungroup the data set
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the circle nest entrance function for the Colony7CircleAggn data set
DistanceCoordsFunctionTrackerCircleAggn7(Colony7CircleAggn)

# BASELINE
DistanceCoordsFunctionTrackerCircleBase7 <- function(data.table){
  CircleCoords <- AggnStudyCircleRefCoords %>% # Reference coordinates for the circle nest
    filter(Colony == "7" & Trial == "Pre" & Nest == "Circle") %>% # Filtering out the baseline assay references
    dplyr::select(c("Coord", "XREF", "YREF")) # Select only the desired columns
  
  # X axis references
  XCoords <- CircleCoords %>% # Creating a x axis reference data set
    dplyr::select(-c("YREF")) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  
  # Y axis references
  YCoords <- CircleCoords %>% # Creating a y axis reference data set
    dplyr::select(-c("XREF")) # Remove x axis references
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # X axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # X axis reference from column 2 from above
  MaxDist = abs(YREF1 - YREF2) # Distance from the back of the nest to the entrance
  
  Colony7CirclePreDist <<- data.table %>% # Data table to run through the function
    mutate(DistanceX = XREF2 - X, # X axis distance to the entrance
           DistanceY = max(Y) - Y, # Y axis distance to the entrance
           PythagDist = sqrt((DistanceX^2) + (DistanceY^2)), # Shortest distance to the nest entrance 
           ScaledDist = PythagDist / MaxDist, # Scaling the distances by the max possible distance
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>% # If any distance is greater than 1 (precautionary), then it's assigned a 1
    ungroup() %>% # Ungroup the data set
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the circle nest entrance function for the Colony7CirclePre data set
DistanceCoordsFunctionTrackerCircleBase7(Colony7CirclePre)

# COLONY 8
# TUBE NEST
# INVADER
DistanceCoordsFunctionTrackerTubeAggn8 <- function(data.table){
  TubeCoords <- AggnStudyRefCoords %>% # Reference data set for eight even area bins
    filter(Colony == "8" & Trial == "Aggn" & Nest == "Tube") %>% # Filter out only tube nest aggression assay references
    dplyr::select(c(Coord, XREF, YREF)) # Select only desired columns 
  
  # X axis references
  XCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(YREF)) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF1 = as.numeric(XCoords[,1]) # X axis reference from column 1 from above
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  XREF3 = as.numeric(XCoords[,3]) # X axis reference from column 3 from above
  XREF4 = as.numeric(XCoords[,4]) # X axis reference from column 4 from above
  XREF5 = as.numeric(XCoords[,5]) # X axis reference from column 5 from above
  XREF6 = as.numeric(XCoords[,6]) # X axis reference from column 6 from above
  XREF7 = as.numeric(XCoords[,7]) # X axis reference from column 7 from above
  
  # Y axis references
  YCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(XREF)) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # Y axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # Y axis reference from column 2 from above
  YREF3 = as.numeric(YCoords[,3]) # Y axis reference from column 3 from above
  YREF4 = as.numeric(YCoords[,4]) # Y axis reference from column 4 from above
  YREF5 = as.numeric(YCoords[,5]) # Y axis reference from column 5 from above
  YREF6 = as.numeric(YCoords[,6]) # Y axis reference from column 6 from above
  YREF7 = as.numeric(YCoords[,7]) # Y axis reference from column 7 from above
  
  # Width of the tube nest
  Width = YREF2 - YREF3
  
  # Reference distances within the tube nest
  Dist1 = sqrt(((XREF1 - XREF2)^2) + ((YREF1 - YREF2)^2)) # Distance to the entrance from corner entering bin 1 from bin 2
  Dist2 = sqrt(((XREF2 - XREF3)^2) + ((YREF2 - YREF3)^2)) + Dist1 # Distance to the entrance from bin 3 entering bin 2
  Dist3 = sqrt(((XREF4 - XREF3)^2) + ((YREF3 - YREF4)^2)) + Dist2 # Distance to the entrance from bin 4, 5, 6 entering bin 3
  Dist4 = sqrt(((XREF5 - XREF4)^2) + ((YREF5 - YREF4)^2)) + Dist3 # Distance to the entrance from bin 7 entering bin 6
  Dist5 = sqrt(((XREF6 - XREF5)^2) + ((YREF6 - YREF5)^2)) + Dist4 # Distance to the entrance from bin 8 entering bin 7
  MaxDist = sqrt(((XREF6 - XREF7)^2) + ((YREF6 - YREF7)^2)) + Dist5 # Distance from the back of the nest to the entrance
  
  Colony8TubeAggnDist <<- data.table %>% # Data table to run through the function
    filter(Nest == "Tube") %>% # Filter out tube nest coordinates
    # Coordinate bin assignment
    mutate(X1 = ifelse(X >= XREF2 & X <= (XREF2 + (1.25 * Width)) & Y >= YREF3, # If the x axiss of each coordinate is in bin 1
                       "YES", "NO"), 
           X2 = ifelse(X < XREF2 & Y >= YREF3, # If the x axis of each coordinate is in bin 2
                       "YES", "NO"),
           X3 = ifelse(X <= XREF4 & Y < YREF3, # If the x axis of each coordinate is in bin 3
                       "YES", "NO"),
           X4 = ifelse(X > XREF4 & Y <= YREF5, # If the x axis of each coordinate is in bins 4, 5, or 6
                       "YES", "NO"),
           X5 = ifelse(X >= XREF6 & Y > YREF5, # If the x axis of each coordinate is in bin 7
                       "YES", "NO"),
           X6 = ifelse(X >= XREF7 & Y >= YREF7, # If the x axis of each coordinate is in bin 8
                       "YES", "NO"),
           Y1 = ifelse(X1 == "YES", "YES", "NO"), # If the coordinate qualified under X1
           Y2 = ifelse(X2 == "YES", "YES", "NO"), # If the coordinate qualified under X2
           Y3 = ifelse(X3 == "YES", "YES", "NO"), # If the coordinate qualified under X3
           Y4 = ifelse(X4 == "YES", "YES", "NO"), # If the coordinate qualified under X4
           Y5 = ifelse(X5 == "YES", "YES", "NO"), # If the coordinate qualified under X5
           Y6 = ifelse(X6 == "YES", "YES", "NO"), # If the coordinate qualified under X6
           # X axis distances
           DistanceX = ifelse(X1 == "YES", # Bin 1
                              X - XREF1,
                              ifelse(X2 == "YES", # Bin 2
                                     XREF2 - X,
                                     ifelse(X3 == "YES", # Bin 3
                                            XREF3 - X,
                                            ifelse(X4 == "YES", # Bins 4-6
                                                   X - XREF4,
                                                   ifelse(X5 == "YES", # Bin 7
                                                          X - XREF5,
                                                          ifelse(X6 == "YES", # Bin 8
                                                                 XREF6 - X,
                                                                 NA)))))), # Else NA
           # Y axis distances
           DistanceY = ifelse(Y1 == "YES", # Bin 1
                              YREF1 - Y,
                              ifelse(Y2 == "YES", # Bin 2
                                     YREF2 - Y,
                                     ifelse(Y3 == "YES" | Y4 == "YES" & X < XREF4, # Bin 3, or bin 4 with x coordinates less than the bin's x axis reference
                                            YREF3 - Y,
                                            ifelse(Y4 == "YES", # Bin 4-6
                                                   YREF4 - Y,
                                                   ifelse(Y5 == "YES", # Bin 7
                                                          Y - YREF5,
                                                          ifelse(Y6 == "YES", # Bin 8
                                                                 YREF6 - Y,
                                                                 NA)))))),
           # Shortest distances from the nest entrance
           # Only bin 1 is a direct line to the entrance, every other bin is the shortest distance to the closest bin to the entrance + reference distance for that bin to the entrance
           PythagDist = ifelse(X1 == "YES" & Y1 == "YES", # Bin 1
                               sqrt((DistanceX^2) + (DistanceY^2)),
                               ifelse(X2 == "YES" & Y2 == "YES", # Bin 2
                                      sqrt((DistanceX^2) + (DistanceY^2)) + Dist1,
                                      ifelse(X3 == "YES" & Y3 == "YES", # Bin 3
                                             sqrt((DistanceX^2) + (DistanceY^2)) + Dist2,
                                             ifelse(X4 == "YES" & Y4 == "YES", # Bin 4-6
                                                    sqrt((DistanceX^2) + (DistanceY^2)) + Dist3, 
                                                    ifelse(X5 == "YES" & Y5 == "YES", # Bin 7
                                                           sqrt((DistanceX^2) + (DistanceY^2)) + Dist4,
                                                           ifelse(X6 == "YES" & Y6 == "YES", # Bin 8
                                                                  sqrt((DistanceX^2) + (DistanceY^2)) + Dist5,
                                                                  NA)))))),
           # Scaling distances by the max possible distance
           ScaledDist = PythagDist / MaxDist,
           # If any distance is greater than 1 (precautionary), then it's assigned a 1
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>%
    ungroup() %>% # Ungroup the data
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the tube nest entrance function for the Colony8TubeAggn data set
DistanceCoordsFunctionTrackerTubeAggn8(Colony8TubeAggn)

# BASELINE
DistanceCoordsFunctionTrackerTubeBase8 <- function(data.table){
  TubeCoords <- AggnStudyRefCoords %>% # Reference data set for eight even area bins
    filter(Colony == "8" & Trial == "Pre" & Nest == "Tube") %>% # Filter out only tube nest baseline assay references
    dplyr::select(c(Coord, XREF, YREF)) # Select only desired columns 
  
  # X axis references
  XCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(YREF)) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF1 = as.numeric(XCoords[,1]) # X axis reference from column 1 from above
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  XREF3 = as.numeric(XCoords[,3]) # X axis reference from column 3 from above
  XREF4 = as.numeric(XCoords[,4]) # X axis reference from column 4 from above
  XREF5 = as.numeric(XCoords[,5]) # X axis reference from column 5 from above
  XREF6 = as.numeric(XCoords[,6]) # X axis reference from column 6 from above
  XREF7 = as.numeric(XCoords[,7]) # X axis reference from column 7 from above
  
  # Y axis references
  YCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(XREF)) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # Y axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # Y axis reference from column 2 from above
  YREF3 = as.numeric(YCoords[,3]) # Y axis reference from column 3 from above
  YREF4 = as.numeric(YCoords[,4]) # Y axis reference from column 4 from above
  YREF5 = as.numeric(YCoords[,5]) # Y axis reference from column 5 from above
  YREF6 = as.numeric(YCoords[,6]) # Y axis reference from column 6 from above
  YREF7 = as.numeric(YCoords[,7]) # Y axis reference from column 7 from above
  
  # Width of the tube nest
  Width = YREF2 - YREF3
  
  # Reference distances within the tube nest
  Dist1 = sqrt(((XREF1 - XREF2)^2) + ((YREF1 - YREF2)^2)) # Distance to the entrance from corner entering bin 1 from bin 2
  Dist2 = sqrt(((XREF2 - XREF3)^2) + ((YREF2 - YREF3)^2)) + Dist1 # Distance to the entrance from bin 3 entering bin 2
  Dist3 = sqrt(((XREF4 - XREF3)^2) + ((YREF3 - YREF4)^2)) + Dist2 # Distance to the entrance from bin 4, 5, 6 entering bin 3
  Dist4 = sqrt(((XREF5 - XREF4)^2) + ((YREF5 - YREF4)^2)) + Dist3 # Distance to the entrance from bin 7 entering bin 6
  Dist5 = sqrt(((XREF6 - XREF5)^2) + ((YREF6 - YREF5)^2)) + Dist4 # Distance to the entrance from bin 8 entering bin 7
  MaxDist = sqrt(((XREF6 - XREF7)^2) + ((YREF6 - YREF7)^2)) + Dist5 # Distance from the back of the nest to the entrance
  
  Colony8TubePreDist <<- data.table %>% # Data table to run through the function
    filter(Nest == "Tube") %>% # Filter out tube nest coordinates
    # Coordinate bin assignment
    mutate(X1 = ifelse(X >= XREF2 & X <= (XREF2 + (1.25 * Width)) & Y >= YREF3, # If the x axiss of each coordinate is in bin 1
                       "YES", "NO"), 
           X2 = ifelse(X < XREF2 & Y >= YREF3, # If the x axis of each coordinate is in bin 2
                       "YES", "NO"),
           X3 = ifelse(X <= XREF4 & Y < YREF3, # If the x axis of each coordinate is in bin 3
                       "YES", "NO"),
           X4 = ifelse(X > XREF4 & Y <= YREF5, # If the x axis of each coordinate is in bins 4, 5, or 6
                       "YES", "NO"),
           X5 = ifelse(X >= XREF6 & Y > YREF5, # If the x axis of each coordinate is in bin 7
                       "YES", "NO"),
           X6 = ifelse(X >= XREF7 & Y >= YREF7, # If the x axis of each coordinate is in bin 8
                       "YES", "NO"),
           Y1 = ifelse(X1 == "YES", "YES", "NO"), # If the coordinate qualified under X1
           Y2 = ifelse(X2 == "YES", "YES", "NO"), # If the coordinate qualified under X2
           Y3 = ifelse(X3 == "YES", "YES", "NO"), # If the coordinate qualified under X3
           Y4 = ifelse(X4 == "YES", "YES", "NO"), # If the coordinate qualified under X4
           Y5 = ifelse(X5 == "YES", "YES", "NO"), # If the coordinate qualified under X5
           Y6 = ifelse(X6 == "YES", "YES", "NO"), # If the coordinate qualified under X6
           # X axis distances
           DistanceX = ifelse(X1 == "YES", # Bin 1
                              X - XREF1,
                              ifelse(X2 == "YES", # Bin 2
                                     XREF2 - X,
                                     ifelse(X3 == "YES", # Bin 3
                                            XREF3 - X,
                                            ifelse(X4 == "YES", # Bins 4-6
                                                   X - XREF4,
                                                   ifelse(X5 == "YES", # Bin 7
                                                          X - XREF5,
                                                          ifelse(X6 == "YES", # Bin 8
                                                                 XREF6 - X,
                                                                 NA)))))), # Else NA
           # Y axis distances
           DistanceY = ifelse(Y1 == "YES", # Bin 1
                              YREF1 - Y,
                              ifelse(Y2 == "YES", # Bin 2
                                     YREF2 - Y,
                                     ifelse(Y3 == "YES" | Y4 == "YES" & X < XREF4, # Bin 3, or bin 4 with x coordinates less than the bin's x axis reference
                                            YREF3 - Y,
                                            ifelse(Y4 == "YES", # Bin 4-6
                                                   YREF4 - Y,
                                                   ifelse(Y5 == "YES", # Bin 7
                                                          Y - YREF5,
                                                          ifelse(Y6 == "YES", # Bin 8
                                                                 YREF6 - Y,
                                                                 NA)))))),
           # Shortest distances from the nest entrance
           # Only bin 1 is a direct line to the entrance, every other bin is the shortest distance to the closest bin to the entrance + reference distance for that bin to the entrance
           PythagDist = ifelse(X1 == "YES" & Y1 == "YES", # Bin 1
                               sqrt((DistanceX^2) + (DistanceY^2)),
                               ifelse(X2 == "YES" & Y2 == "YES", # Bin 2
                                      sqrt((DistanceX^2) + (DistanceY^2)) + Dist1,
                                      ifelse(X3 == "YES" & Y3 == "YES", # Bin 3
                                             sqrt((DistanceX^2) + (DistanceY^2)) + Dist2,
                                             ifelse(X4 == "YES" & Y4 == "YES", # Bin 4-6
                                                    sqrt((DistanceX^2) + (DistanceY^2)) + Dist3, 
                                                    ifelse(X5 == "YES" & Y5 == "YES", # Bin 7
                                                           sqrt((DistanceX^2) + (DistanceY^2)) + Dist4,
                                                           ifelse(X6 == "YES" & Y6 == "YES", # Bin 8
                                                                  sqrt((DistanceX^2) + (DistanceY^2)) + Dist5,
                                                                  NA)))))),
           # Scaling distances by the max possible distance
           ScaledDist = PythagDist / MaxDist,
           # If any distance is greater than 1 (precautionary), then it's assigned a 1
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>%
    ungroup() %>% # Ungroup the data
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the tube nest entrance function for the Colony8TubePre data set
DistanceCoordsFunctionTrackerTubeBase8(Colony8TubePre)

# CIRCLE NEST
# INVADER
DistanceCoordsFunctionTrackerCircleAggn8 <-function(data.table){
  CircleCoords <- AggnStudyCircleRefCoords %>% # Reference coordinates for the circle nest
    filter(Colony == "8" & Trial == "Aggn" & Nest == "Circle") %>% # Filtering out the aggression assay references
    dplyr::select(c("Coord", "XREF", "YREF")) # Select only the desired columns
  
  # X axis references
  XCoords <- CircleCoords %>% # Creating a x axis reference data set
    dplyr::select(-c("YREF")) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  
  # Y axis references
  YCoords <- CircleCoords %>% # Creating a y axis reference data set
    dplyr::select(-c("XREF")) # Remove x axis references
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # X axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # X axis reference from column 2 from above
  MaxDist = abs(YREF1 - YREF2) # Distance from the back of the nest to the entrance
  
  Colony8CircleAggnDist <<- data.table %>% # Data table to run through the function
    mutate(DistanceX = XREF2 - X, # X axis distance to the entrance
           DistanceY = max(Y) - Y, # Y axis distance to the entrance
           PythagDist = sqrt((DistanceX^2) + (DistanceY^2)), # Shortest distance to the nest entrance 
           ScaledDist = PythagDist / MaxDist, # Scaling the distances by the max possible distance
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>% # If any distance is greater than 1 (precautionary), then it's assigned a 1
    ungroup() %>% # Ungroup the data set
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the circle nest entrance function for the Colony8CircleAggn data set
DistanceCoordsFunctionTrackerCircleAggn8(Colony8CircleAggn)

# BASELINE
DistanceCoordsFunctionTrackerCircleBase8 <- function(data.table){
  CircleCoords <- AggnStudyCircleRefCoords %>% # Reference coordinates for the circle nest
    filter(Colony == "8" & Trial == "Pre" & Nest == "Circle") %>% # Filtering out the baseline assay references
    dplyr::select(c("Coord", "XREF", "YREF")) # Select only the desired columns
  
  # X axis references
  XCoords <- CircleCoords %>% # Creating a x axis reference data set
    dplyr::select(-c("YREF")) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  
  # Y axis references
  YCoords <- CircleCoords %>% # Creating a y axis reference data set
    dplyr::select(-c("XREF")) # Remove x axis references
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # X axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # X axis reference from column 2 from above
  MaxDist = abs(YREF1 - YREF2) # Distance from the back of the nest to the entrance
  
  Colony8CirclePreDist <<- data.table %>% # Data table to run through the function
    mutate(DistanceX = XREF2 - X, # X axis distance to the entrance
           DistanceY = max(Y) - Y, # Y axis distance to the entrance
           PythagDist = sqrt((DistanceX^2) + (DistanceY^2)), # Shortest distance to the nest entrance 
           ScaledDist = PythagDist / MaxDist, # Scaling the distances by the max possible distance
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>% # If any distance is greater than 1 (precautionary), then it's assigned a 1
    ungroup() %>% # Ungroup the data set
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the circle nest entrance function for the Colony8CirclePre data set
DistanceCoordsFunctionTrackerCircleBase8(Colony8CirclePre)

# COLONY 9
# TUBE NEST
# INVADER
DistanceCoordsFunctionTrackerTubeAggn9 <- function(data.table){
  TubeCoords <- AggnStudyRefCoords %>% # Reference data set for eight even area bins
    filter(Colony == "9" & Trial == "Aggn" & Nest == "Tube") %>% # Filter out only tube nest aggression assay references
    dplyr::select(c(Coord, XREF, YREF)) # Select only desired columns 
  
  # X axis references
  XCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(YREF)) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF1 = as.numeric(XCoords[,1]) # X axis reference from column 1 from above
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  XREF3 = as.numeric(XCoords[,3]) # X axis reference from column 3 from above
  XREF4 = as.numeric(XCoords[,4]) # X axis reference from column 4 from above
  XREF5 = as.numeric(XCoords[,5]) # X axis reference from column 5 from above
  XREF6 = as.numeric(XCoords[,6]) # X axis reference from column 6 from above
  XREF7 = as.numeric(XCoords[,7]) # X axis reference from column 7 from above
  
  # Y axis references
  YCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(XREF)) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # Y axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # Y axis reference from column 2 from above
  YREF3 = as.numeric(YCoords[,3]) # Y axis reference from column 3 from above
  YREF4 = as.numeric(YCoords[,4]) # Y axis reference from column 4 from above
  YREF5 = as.numeric(YCoords[,5]) # Y axis reference from column 5 from above
  YREF6 = as.numeric(YCoords[,6]) # Y axis reference from column 6 from above
  YREF7 = as.numeric(YCoords[,7]) # Y axis reference from column 7 from above
  
  # Width of the tube nest
  Width = YREF2 - YREF3
  
  # Reference distances within the tube nest
  Dist1 = sqrt(((XREF1 - XREF2)^2) + ((YREF1 - YREF2)^2)) # Distance to the entrance from corner entering bin 1 from bin 2
  Dist2 = sqrt(((XREF2 - XREF3)^2) + ((YREF2 - YREF3)^2)) + Dist1 # Distance to the entrance from bin 3 entering bin 2
  Dist3 = sqrt(((XREF4 - XREF3)^2) + ((YREF3 - YREF4)^2)) + Dist2 # Distance to the entrance from bin 4, 5, 6 entering bin 3
  Dist4 = sqrt(((XREF5 - XREF4)^2) + ((YREF5 - YREF4)^2)) + Dist3 # Distance to the entrance from bin 7 entering bin 6
  Dist5 = sqrt(((XREF6 - XREF5)^2) + ((YREF6 - YREF5)^2)) + Dist4 # Distance to the entrance from bin 8 entering bin 7
  MaxDist = sqrt(((XREF6 - XREF7)^2) + ((YREF6 - YREF7)^2)) + Dist5 # Distance from the back of the nest to the entrance
  
  Colony9TubeAggnDist <<- data.table %>% # Data table to run through the function
    filter(Nest == "Tube") %>% # Filter out tube nest coordinates
    # Coordinate bin assignment
    mutate(X1 = ifelse(X >= XREF2 & X <= (XREF2 + (1.25 * Width)) & Y >= YREF3, # If the x axiss of each coordinate is in bin 1
                       "YES", "NO"), 
           X2 = ifelse(X < XREF2 & Y >= YREF3, # If the x axis of each coordinate is in bin 2
                       "YES", "NO"),
           X3 = ifelse(X <= XREF4 & Y < YREF3, # If the x axis of each coordinate is in bin 3
                       "YES", "NO"),
           X4 = ifelse(X > XREF4 & Y <= YREF5, # If the x axis of each coordinate is in bins 4, 5, or 6
                       "YES", "NO"),
           X5 = ifelse(X >= XREF6 & Y > YREF5, # If the x axis of each coordinate is in bin 7
                       "YES", "NO"),
           X6 = ifelse(X >= XREF7 & Y >= YREF7, # If the x axis of each coordinate is in bin 8
                       "YES", "NO"),
           Y1 = ifelse(X1 == "YES", "YES", "NO"), # If the coordinate qualified under X1
           Y2 = ifelse(X2 == "YES", "YES", "NO"), # If the coordinate qualified under X2
           Y3 = ifelse(X3 == "YES", "YES", "NO"), # If the coordinate qualified under X3
           Y4 = ifelse(X4 == "YES", "YES", "NO"), # If the coordinate qualified under X4
           Y5 = ifelse(X5 == "YES", "YES", "NO"), # If the coordinate qualified under X5
           Y6 = ifelse(X6 == "YES", "YES", "NO"), # If the coordinate qualified under X6
           # X axis distances
           DistanceX = ifelse(X1 == "YES", # Bin 1
                              X - XREF1,
                              ifelse(X2 == "YES", # Bin 2
                                     XREF2 - X,
                                     ifelse(X3 == "YES", # Bin 3
                                            XREF3 - X,
                                            ifelse(X4 == "YES", # Bins 4-6
                                                   X - XREF4,
                                                   ifelse(X5 == "YES", # Bin 7
                                                          X - XREF5,
                                                          ifelse(X6 == "YES", # Bin 8
                                                                 XREF6 - X,
                                                                 NA)))))), # Else NA
           # Y axis distances
           DistanceY = ifelse(Y1 == "YES", # Bin 1
                              YREF1 - Y,
                              ifelse(Y2 == "YES", # Bin 2
                                     YREF2 - Y,
                                     ifelse(Y3 == "YES" | Y4 == "YES" & X < XREF4, # Bin 3, or bin 4 with x coordinates less than the bin's x axis reference
                                            YREF3 - Y,
                                            ifelse(Y4 == "YES", # Bin 4-6
                                                   YREF4 - Y,
                                                   ifelse(Y5 == "YES", # Bin 7
                                                          Y - YREF5,
                                                          ifelse(Y6 == "YES", # Bin 8
                                                                 YREF6 - Y,
                                                                 NA)))))),
           # Shortest distances from the nest entrance
           # Only bin 1 is a direct line to the entrance, every other bin is the shortest distance to the closest bin to the entrance + reference distance for that bin to the entrance
           PythagDist = ifelse(X1 == "YES" & Y1 == "YES", # Bin 1
                               sqrt((DistanceX^2) + (DistanceY^2)),
                               ifelse(X2 == "YES" & Y2 == "YES", # Bin 2
                                      sqrt((DistanceX^2) + (DistanceY^2)) + Dist1,
                                      ifelse(X3 == "YES" & Y3 == "YES", # Bin 3
                                             sqrt((DistanceX^2) + (DistanceY^2)) + Dist2,
                                             ifelse(X4 == "YES" & Y4 == "YES", # Bin 4-6
                                                    sqrt((DistanceX^2) + (DistanceY^2)) + Dist3, 
                                                    ifelse(X5 == "YES" & Y5 == "YES", # Bin 7
                                                           sqrt((DistanceX^2) + (DistanceY^2)) + Dist4,
                                                           ifelse(X6 == "YES" & Y6 == "YES", # Bin 8
                                                                  sqrt((DistanceX^2) + (DistanceY^2)) + Dist5,
                                                                  NA)))))),
           # Scaling distances by the max possible distance
           ScaledDist = PythagDist / MaxDist,
           # If any distance is greater than 1 (precautionary), then it's assigned a 1
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>%
    ungroup() %>% # Ungroup the data
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the tube nest entrance function for the Colony9TubeAggn data set
DistanceCoordsFunctionTrackerTubeAggn9(Colony9TubeAggn)

# BASELINE
DistanceCoordsFunctionTrackerTubeBase9 <- function(data.table){
  TubeCoords <- AggnStudyRefCoords %>% # Reference data set for eight even area bins
    filter(Colony == "9" & Trial == "Pre" & Nest == "Tube") %>% # Filter out only tube nest baseline assay references
    dplyr::select(c(Coord, XREF, YREF)) # Select only desired columns 
  
  # X axis references
  XCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(YREF)) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF1 = as.numeric(XCoords[,1]) # X axis reference from column 1 from above
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  XREF3 = as.numeric(XCoords[,3]) # X axis reference from column 3 from above
  XREF4 = as.numeric(XCoords[,4]) # X axis reference from column 4 from above
  XREF5 = as.numeric(XCoords[,5]) # X axis reference from column 5 from above
  XREF6 = as.numeric(XCoords[,6]) # X axis reference from column 6 from above
  XREF7 = as.numeric(XCoords[,7]) # X axis reference from column 7 from above
  
  # Y axis references
  YCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(XREF)) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # Y axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # Y axis reference from column 2 from above
  YREF3 = as.numeric(YCoords[,3]) # Y axis reference from column 3 from above
  YREF4 = as.numeric(YCoords[,4]) # Y axis reference from column 4 from above
  YREF5 = as.numeric(YCoords[,5]) # Y axis reference from column 5 from above
  YREF6 = as.numeric(YCoords[,6]) # Y axis reference from column 6 from above
  YREF7 = as.numeric(YCoords[,7]) # Y axis reference from column 7 from above
  
  # Width of the tube nest
  Width = YREF2 - YREF3
  
  # Reference distances within the tube nest
  Dist1 = sqrt(((XREF1 - XREF2)^2) + ((YREF1 - YREF2)^2)) # Distance to the entrance from corner entering bin 1 from bin 2
  Dist2 = sqrt(((XREF2 - XREF3)^2) + ((YREF2 - YREF3)^2)) + Dist1 # Distance to the entrance from bin 3 entering bin 2
  Dist3 = sqrt(((XREF4 - XREF3)^2) + ((YREF3 - YREF4)^2)) + Dist2 # Distance to the entrance from bin 4, 5, 6 entering bin 3
  Dist4 = sqrt(((XREF5 - XREF4)^2) + ((YREF5 - YREF4)^2)) + Dist3 # Distance to the entrance from bin 7 entering bin 6
  Dist5 = sqrt(((XREF6 - XREF5)^2) + ((YREF6 - YREF5)^2)) + Dist4 # Distance to the entrance from bin 8 entering bin 7
  MaxDist = sqrt(((XREF6 - XREF7)^2) + ((YREF6 - YREF7)^2)) + Dist5 # Distance from the back of the nest to the entrance
  
  Colony9TubePreDist <<- data.table %>% # Data table to run through the function
    filter(Nest == "Tube") %>% # Filter out tube nest coordinates
    # Coordinate bin assignment
    mutate(X1 = ifelse(X >= XREF2 & X <= (XREF2 + (1.25 * Width)) & Y >= YREF3, # If the x axiss of each coordinate is in bin 1
                       "YES", "NO"), 
           X2 = ifelse(X < XREF2 & Y >= YREF3, # If the x axis of each coordinate is in bin 2
                       "YES", "NO"),
           X3 = ifelse(X <= XREF4 & Y < YREF3, # If the x axis of each coordinate is in bin 3
                       "YES", "NO"),
           X4 = ifelse(X > XREF4 & Y <= YREF5, # If the x axis of each coordinate is in bins 4, 5, or 6
                       "YES", "NO"),
           X5 = ifelse(X >= XREF6 & Y > YREF5, # If the x axis of each coordinate is in bin 7
                       "YES", "NO"),
           X6 = ifelse(X >= XREF7 & Y >= YREF7, # If the x axis of each coordinate is in bin 8
                       "YES", "NO"),
           Y1 = ifelse(X1 == "YES", "YES", "NO"), # If the coordinate qualified under X1
           Y2 = ifelse(X2 == "YES", "YES", "NO"), # If the coordinate qualified under X2
           Y3 = ifelse(X3 == "YES", "YES", "NO"), # If the coordinate qualified under X3
           Y4 = ifelse(X4 == "YES", "YES", "NO"), # If the coordinate qualified under X4
           Y5 = ifelse(X5 == "YES", "YES", "NO"), # If the coordinate qualified under X5
           Y6 = ifelse(X6 == "YES", "YES", "NO"), # If the coordinate qualified under X6
           # X axis distances
           DistanceX = ifelse(X1 == "YES", # Bin 1
                              X - XREF1,
                              ifelse(X2 == "YES", # Bin 2
                                     XREF2 - X,
                                     ifelse(X3 == "YES", # Bin 3
                                            XREF3 - X,
                                            ifelse(X4 == "YES", # Bins 4-6
                                                   X - XREF4,
                                                   ifelse(X5 == "YES", # Bin 7
                                                          X - XREF5,
                                                          ifelse(X6 == "YES", # Bin 8
                                                                 XREF6 - X,
                                                                 NA)))))), # Else NA
           # Y axis distances
           DistanceY = ifelse(Y1 == "YES", # Bin 1
                              YREF1 - Y,
                              ifelse(Y2 == "YES", # Bin 2
                                     YREF2 - Y,
                                     ifelse(Y3 == "YES" | Y4 == "YES" & X < XREF4, # Bin 3, or bin 4 with x coordinates less than the bin's x axis reference
                                            YREF3 - Y,
                                            ifelse(Y4 == "YES", # Bin 4-6
                                                   YREF4 - Y,
                                                   ifelse(Y5 == "YES", # Bin 7
                                                          Y - YREF5,
                                                          ifelse(Y6 == "YES", # Bin 8
                                                                 YREF6 - Y,
                                                                 NA)))))),
           # Shortest distances from the nest entrance
           # Only bin 1 is a direct line to the entrance, every other bin is the shortest distance to the closest bin to the entrance + reference distance for that bin to the entrance
           PythagDist = ifelse(X1 == "YES" & Y1 == "YES", # Bin 1
                               sqrt((DistanceX^2) + (DistanceY^2)),
                               ifelse(X2 == "YES" & Y2 == "YES", # Bin 2
                                      sqrt((DistanceX^2) + (DistanceY^2)) + Dist1,
                                      ifelse(X3 == "YES" & Y3 == "YES", # Bin 3
                                             sqrt((DistanceX^2) + (DistanceY^2)) + Dist2,
                                             ifelse(X4 == "YES" & Y4 == "YES", # Bin 4-6
                                                    sqrt((DistanceX^2) + (DistanceY^2)) + Dist3, 
                                                    ifelse(X5 == "YES" & Y5 == "YES", # Bin 7
                                                           sqrt((DistanceX^2) + (DistanceY^2)) + Dist4,
                                                           ifelse(X6 == "YES" & Y6 == "YES", # Bin 8
                                                                  sqrt((DistanceX^2) + (DistanceY^2)) + Dist5,
                                                                  NA)))))),
           # Scaling distances by the max possible distance
           ScaledDist = PythagDist / MaxDist,
           # If any distance is greater than 1 (precautionary), then it's assigned a 1
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>%
    ungroup() %>% # Ungroup the data
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the tube nest entrance function for the Colony9TubePre data set
DistanceCoordsFunctionTrackerTubeBase9(Colony9TubePre)

# CIRCLE NEST
# INVADER
DistanceCoordsFunctionTrackerCircleAggn9 <-function(data.table){
  CircleCoords <- AggnStudyCircleRefCoords %>% # Reference coordinates for the circle nest
    filter(Colony == "9" & Trial == "Aggn" & Nest == "Circle") %>% # Filtering out the aggression assay references
    dplyr::select(c("Coord", "XREF", "YREF")) # Select only the desired columns
  
  # X axis references
  XCoords <- CircleCoords %>% # Creating a x axis reference data set
    dplyr::select(-c("YREF")) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  
  # Y axis references
  YCoords <- CircleCoords %>% # Creating a y axis reference data set
    dplyr::select(-c("XREF")) # Remove x axis references
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # X axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # X axis reference from column 2 from above
  MaxDist = abs(YREF1 - YREF2) # Distance from the back of the nest to the entrance
  
  Colony9CircleAggnDist <<- data.table %>% # Data table to run through the function
    mutate(DistanceX = XREF2 - X, # X axis distance to the entrance
           DistanceY = max(Y) - Y, # Y axis distance to the entrance
           PythagDist = sqrt((DistanceX^2) + (DistanceY^2)), # Shortest distance to the nest entrance 
           ScaledDist = PythagDist / MaxDist, # Scaling the distances by the max possible distance
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>% # If any distance is greater than 1 (precautionary), then it's assigned a 1
    ungroup() %>% # Ungroup the data set
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the circle nest entrance function for the Colony9CircleAggn data set
DistanceCoordsFunctionTrackerCircleAggn9(Colony9CircleAggn)

# BASELINE
DistanceCoordsFunctionTrackerCircleBase9 <- function(data.table){
  CircleCoords <- AggnStudyCircleRefCoords %>% # Reference coordinates for the circle nest
    filter(Colony == "9" & Trial == "Pre" & Nest == "Circle") %>% # Filtering out the baseline assay references
    dplyr::select(c("Coord", "XREF", "YREF")) # Select only the desired columns
  
  # X axis references
  XCoords <- CircleCoords %>% # Creating a x axis reference data set
    dplyr::select(-c("YREF")) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  
  # Y axis references
  YCoords <- CircleCoords %>% # Creating a y axis reference data set
    dplyr::select(-c("XREF")) # Remove x axis references
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # X axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # X axis reference from column 2 from above
  MaxDist = abs(YREF1 - YREF2) # Distance from the back of the nest to the entrance
  
  Colony9CirclePreDist <<- data.table %>% # Data table to run through the function
    mutate(DistanceX = XREF2 - X, # X axis distance to the entrance
           DistanceY = max(Y) - Y, # Y axis distance to the entrance
           PythagDist = sqrt((DistanceX^2) + (DistanceY^2)), # Shortest distance to the nest entrance 
           ScaledDist = PythagDist / MaxDist, # Scaling the distances by the max possible distance
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>% # If any distance is greater than 1 (precautionary), then it's assigned a 1
    ungroup() %>% # Ungroup the data set
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the circle nest entrance function for the Colony9CirclePre data set
DistanceCoordsFunctionTrackerCircleBase9(Colony9CirclePre)

# COLONY 11
# TUBE NEST
# INVADER
DistanceCoordsFunctionTrackerTubeAggn11 <- function(data.table){
  TubeCoords <- AggnStudyRefCoords %>% # Reference data set for eight even area bins
    filter(Colony == "11" & Trial == "Aggn" & Nest == "Tube") %>% # Filter out only tube nest aggression assay references
    dplyr::select(c(Coord, XREF, YREF)) # Select only desired columns 
  
  # X axis references
  XCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(YREF)) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF1 = as.numeric(XCoords[,1]) # X axis reference from column 1 from above
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  XREF3 = as.numeric(XCoords[,3]) # X axis reference from column 3 from above
  XREF4 = as.numeric(XCoords[,4]) # X axis reference from column 4 from above
  XREF5 = as.numeric(XCoords[,5]) # X axis reference from column 5 from above
  XREF6 = as.numeric(XCoords[,6]) # X axis reference from column 6 from above
  XREF7 = as.numeric(XCoords[,7]) # X axis reference from column 7 from above
  
  # Y axis references
  YCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(XREF)) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # Y axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # Y axis reference from column 2 from above
  YREF3 = as.numeric(YCoords[,3]) # Y axis reference from column 3 from above
  YREF4 = as.numeric(YCoords[,4]) # Y axis reference from column 4 from above
  YREF5 = as.numeric(YCoords[,5]) # Y axis reference from column 5 from above
  YREF6 = as.numeric(YCoords[,6]) # Y axis reference from column 6 from above
  YREF7 = as.numeric(YCoords[,7]) # Y axis reference from column 7 from above
  
  # Width of the tube nest
  Width = YREF2 - YREF3
  
  # Reference distances within the tube nest
  Dist1 = sqrt(((XREF1 - XREF2)^2) + ((YREF1 - YREF2)^2)) # Distance to the entrance from corner entering bin 1 from bin 2
  Dist2 = sqrt(((XREF2 - XREF3)^2) + ((YREF2 - YREF3)^2)) + Dist1 # Distance to the entrance from bin 3 entering bin 2
  Dist3 = sqrt(((XREF4 - XREF3)^2) + ((YREF3 - YREF4)^2)) + Dist2 # Distance to the entrance from bin 4, 5, 6 entering bin 3
  Dist4 = sqrt(((XREF5 - XREF4)^2) + ((YREF5 - YREF4)^2)) + Dist3 # Distance to the entrance from bin 7 entering bin 6
  Dist5 = sqrt(((XREF6 - XREF5)^2) + ((YREF6 - YREF5)^2)) + Dist4 # Distance to the entrance from bin 8 entering bin 7
  MaxDist = sqrt(((XREF6 - XREF7)^2) + ((YREF6 - YREF7)^2)) + Dist5 # Distance from the back of the nest to the entrance
  
  Colony11TubeAggnDist <<- data.table %>% # Data table to run through the function
    filter(Nest == "Tube") %>% # Filter out tube nest coordinates
    # Coordinate bin assignment
    mutate(X1 = ifelse(X >= XREF2 & X <= (XREF2 + (1.25 * Width)) & Y >= YREF3, # If the x axiss of each coordinate is in bin 1
                       "YES", "NO"), 
           X2 = ifelse(X < XREF2 & Y >= YREF3, # If the x axis of each coordinate is in bin 2
                       "YES", "NO"),
           X3 = ifelse(X <= XREF4 & Y < YREF3, # If the x axis of each coordinate is in bin 3
                       "YES", "NO"),
           X4 = ifelse(X > XREF4 & Y <= YREF5, # If the x axis of each coordinate is in bins 4, 5, or 6
                       "YES", "NO"),
           X5 = ifelse(X >= XREF6 & Y > YREF5, # If the x axis of each coordinate is in bin 7
                       "YES", "NO"),
           X6 = ifelse(X >= XREF7 & Y >= YREF7, # If the x axis of each coordinate is in bin 8
                       "YES", "NO"),
           Y1 = ifelse(X1 == "YES", "YES", "NO"), # If the coordinate qualified under X1
           Y2 = ifelse(X2 == "YES", "YES", "NO"), # If the coordinate qualified under X2
           Y3 = ifelse(X3 == "YES", "YES", "NO"), # If the coordinate qualified under X3
           Y4 = ifelse(X4 == "YES", "YES", "NO"), # If the coordinate qualified under X4
           Y5 = ifelse(X5 == "YES", "YES", "NO"), # If the coordinate qualified under X5
           Y6 = ifelse(X6 == "YES", "YES", "NO"), # If the coordinate qualified under X6
           # X axis distances
           DistanceX = ifelse(X1 == "YES", # Bin 1
                              X - XREF1,
                              ifelse(X2 == "YES", # Bin 2
                                     XREF2 - X,
                                     ifelse(X3 == "YES", # Bin 3
                                            XREF3 - X,
                                            ifelse(X4 == "YES", # Bins 4-6
                                                   X - XREF4,
                                                   ifelse(X5 == "YES", # Bin 7
                                                          X - XREF5,
                                                          ifelse(X6 == "YES", # Bin 8
                                                                 XREF6 - X,
                                                                 NA)))))), # Else NA
           # Y axis distances
           DistanceY = ifelse(Y1 == "YES", # Bin 1
                              YREF1 - Y,
                              ifelse(Y2 == "YES", # Bin 2
                                     YREF2 - Y,
                                     ifelse(Y3 == "YES" | Y4 == "YES" & X < XREF4, # Bin 3, or bin 4 with x coordinates less than the bin's x axis reference
                                            YREF3 - Y,
                                            ifelse(Y4 == "YES", # Bin 4-6
                                                   YREF4 - Y,
                                                   ifelse(Y5 == "YES", # Bin 7
                                                          Y - YREF5,
                                                          ifelse(Y6 == "YES", # Bin 8
                                                                 YREF6 - Y,
                                                                 NA)))))),
           # Shortest distances from the nest entrance
           # Only bin 1 is a direct line to the entrance, every other bin is the shortest distance to the closest bin to the entrance + reference distance for that bin to the entrance
           PythagDist = ifelse(X1 == "YES" & Y1 == "YES", # Bin 1
                               sqrt((DistanceX^2) + (DistanceY^2)),
                               ifelse(X2 == "YES" & Y2 == "YES", # Bin 2
                                      sqrt((DistanceX^2) + (DistanceY^2)) + Dist1,
                                      ifelse(X3 == "YES" & Y3 == "YES", # Bin 3
                                             sqrt((DistanceX^2) + (DistanceY^2)) + Dist2,
                                             ifelse(X4 == "YES" & Y4 == "YES", # Bin 4-6
                                                    sqrt((DistanceX^2) + (DistanceY^2)) + Dist3, 
                                                    ifelse(X5 == "YES" & Y5 == "YES", # Bin 7
                                                           sqrt((DistanceX^2) + (DistanceY^2)) + Dist4,
                                                           ifelse(X6 == "YES" & Y6 == "YES", # Bin 8
                                                                  sqrt((DistanceX^2) + (DistanceY^2)) + Dist5,
                                                                  NA)))))),
           # Scaling distances by the max possible distance
           ScaledDist = PythagDist / MaxDist,
           # If any distance is greater than 1 (precautionary), then it's assigned a 1
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>%
    ungroup() %>% # Ungroup the data
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the tube nest entrance function for the Colony11TubeAggn data set
DistanceCoordsFunctionTrackerTubeAggn11(Colony11TubeAggn)

# BASELINE
DistanceCoordsFunctionTrackerTubeBase11 <- function(data.table){
  TubeCoords <- AggnStudyRefCoords %>% # Reference data set for eight even area bins
    filter(Colony == "11" & Trial == "Pre" & Nest == "Tube") %>% # Filter out only tube nest baseline assay references
    dplyr::select(c(Coord, XREF, YREF)) # Select only desired columns 
  
  # X axis references
  XCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(YREF)) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF1 = as.numeric(XCoords[,1]) # X axis reference from column 1 from above
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  XREF3 = as.numeric(XCoords[,3]) # X axis reference from column 3 from above
  XREF4 = as.numeric(XCoords[,4]) # X axis reference from column 4 from above
  XREF5 = as.numeric(XCoords[,5]) # X axis reference from column 5 from above
  XREF6 = as.numeric(XCoords[,6]) # X axis reference from column 6 from above
  XREF7 = as.numeric(XCoords[,7]) # X axis reference from column 7 from above
  
  # Y axis references
  YCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(XREF)) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # Y axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # Y axis reference from column 2 from above
  YREF3 = as.numeric(YCoords[,3]) # Y axis reference from column 3 from above
  YREF4 = as.numeric(YCoords[,4]) # Y axis reference from column 4 from above
  YREF5 = as.numeric(YCoords[,5]) # Y axis reference from column 5 from above
  YREF6 = as.numeric(YCoords[,6]) # Y axis reference from column 6 from above
  YREF7 = as.numeric(YCoords[,7]) # Y axis reference from column 7 from above
  
  # Width of the tube nest
  Width = YREF2 - YREF3
  
  # Reference distances within the tube nest
  Dist1 = sqrt(((XREF1 - XREF2)^2) + ((YREF1 - YREF2)^2)) # Distance to the entrance from corner entering bin 1 from bin 2
  Dist2 = sqrt(((XREF2 - XREF3)^2) + ((YREF2 - YREF3)^2)) + Dist1 # Distance to the entrance from bin 3 entering bin 2
  Dist3 = sqrt(((XREF4 - XREF3)^2) + ((YREF3 - YREF4)^2)) + Dist2 # Distance to the entrance from bin 4, 5, 6 entering bin 3
  Dist4 = sqrt(((XREF5 - XREF4)^2) + ((YREF5 - YREF4)^2)) + Dist3 # Distance to the entrance from bin 7 entering bin 6
  Dist5 = sqrt(((XREF6 - XREF5)^2) + ((YREF6 - YREF5)^2)) + Dist4 # Distance to the entrance from bin 8 entering bin 7
  MaxDist = sqrt(((XREF6 - XREF7)^2) + ((YREF6 - YREF7)^2)) + Dist5 # Distance from the back of the nest to the entrance
  
  Colony11TubePreDist <<- data.table %>% # Data table to run through the function
    filter(Nest == "Tube") %>% # Filter out tube nest coordinates
    # Coordinate bin assignment
    mutate(X1 = ifelse(X >= XREF2 & X <= (XREF2 + (1.25 * Width)) & Y >= YREF3, # If the x axiss of each coordinate is in bin 1
                       "YES", "NO"), 
           X2 = ifelse(X < XREF2 & Y >= YREF3, # If the x axis of each coordinate is in bin 2
                       "YES", "NO"),
           X3 = ifelse(X <= XREF4 & Y < YREF3, # If the x axis of each coordinate is in bin 3
                       "YES", "NO"),
           X4 = ifelse(X > XREF4 & Y <= YREF5, # If the x axis of each coordinate is in bins 4, 5, or 6
                       "YES", "NO"),
           X5 = ifelse(X >= XREF6 & Y > YREF5, # If the x axis of each coordinate is in bin 7
                       "YES", "NO"),
           X6 = ifelse(X >= XREF7 & Y >= YREF7, # If the x axis of each coordinate is in bin 8
                       "YES", "NO"),
           Y1 = ifelse(X1 == "YES", "YES", "NO"), # If the coordinate qualified under X1
           Y2 = ifelse(X2 == "YES", "YES", "NO"), # If the coordinate qualified under X2
           Y3 = ifelse(X3 == "YES", "YES", "NO"), # If the coordinate qualified under X3
           Y4 = ifelse(X4 == "YES", "YES", "NO"), # If the coordinate qualified under X4
           Y5 = ifelse(X5 == "YES", "YES", "NO"), # If the coordinate qualified under X5
           Y6 = ifelse(X6 == "YES", "YES", "NO"), # If the coordinate qualified under X6
           # X axis distances
           DistanceX = ifelse(X1 == "YES", # Bin 1
                              X - XREF1,
                              ifelse(X2 == "YES", # Bin 2
                                     XREF2 - X,
                                     ifelse(X3 == "YES", # Bin 3
                                            XREF3 - X,
                                            ifelse(X4 == "YES", # Bins 4-6
                                                   X - XREF4,
                                                   ifelse(X5 == "YES", # Bin 7
                                                          X - XREF5,
                                                          ifelse(X6 == "YES", # Bin 8
                                                                 XREF6 - X,
                                                                 NA)))))), # Else NA
           # Y axis distances
           DistanceY = ifelse(Y1 == "YES", # Bin 1
                              YREF1 - Y,
                              ifelse(Y2 == "YES", # Bin 2
                                     YREF2 - Y,
                                     ifelse(Y3 == "YES" | Y4 == "YES" & X < XREF4, # Bin 3, or bin 4 with x coordinates less than the bin's x axis reference
                                            YREF3 - Y,
                                            ifelse(Y4 == "YES", # Bin 4-6
                                                   YREF4 - Y,
                                                   ifelse(Y5 == "YES", # Bin 7
                                                          Y - YREF5,
                                                          ifelse(Y6 == "YES", # Bin 8
                                                                 YREF6 - Y,
                                                                 NA)))))),
           # Shortest distances from the nest entrance
           # Only bin 1 is a direct line to the entrance, every other bin is the shortest distance to the closest bin to the entrance + reference distance for that bin to the entrance
           PythagDist = ifelse(X1 == "YES" & Y1 == "YES", # Bin 1
                               sqrt((DistanceX^2) + (DistanceY^2)),
                               ifelse(X2 == "YES" & Y2 == "YES", # Bin 2
                                      sqrt((DistanceX^2) + (DistanceY^2)) + Dist1,
                                      ifelse(X3 == "YES" & Y3 == "YES", # Bin 3
                                             sqrt((DistanceX^2) + (DistanceY^2)) + Dist2,
                                             ifelse(X4 == "YES" & Y4 == "YES", # Bin 4-6
                                                    sqrt((DistanceX^2) + (DistanceY^2)) + Dist3, 
                                                    ifelse(X5 == "YES" & Y5 == "YES", # Bin 7
                                                           sqrt((DistanceX^2) + (DistanceY^2)) + Dist4,
                                                           ifelse(X6 == "YES" & Y6 == "YES", # Bin 8
                                                                  sqrt((DistanceX^2) + (DistanceY^2)) + Dist5,
                                                                  NA)))))),
           # Scaling distances by the max possible distance
           ScaledDist = PythagDist / MaxDist,
           # If any distance is greater than 1 (precautionary), then it's assigned a 1
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>%
    ungroup() %>% # Ungroup the data
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the tube nest entrance function for the Colony11TubePre data set
DistanceCoordsFunctionTrackerTubeBase11(Colony11TubePre)

# CIRCLE NEST
# INVADER
DistanceCoordsFunctionTrackerCircleAggn11 <-function(data.table){
  CircleCoords <- AggnStudyCircleRefCoords %>% # Reference coordinates for the circle nest
    filter(Colony == "11" & Trial == "Aggn" & Nest == "Circle") %>% # Filtering out the aggression assay references
    dplyr::select(c("Coord", "XREF", "YREF")) # Select only the desired columns
  
  # X axis references
  XCoords <- CircleCoords %>% # Creating a x axis reference data set
    dplyr::select(-c("YREF")) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  
  # Y axis references
  YCoords <- CircleCoords %>% # Creating a y axis reference data set
    dplyr::select(-c("XREF")) # Remove x axis references
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # X axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # X axis reference from column 2 from above
  MaxDist = abs(YREF1 - YREF2) # Distance from the back of the nest to the entrance
  
  Colony11CircleAggnDist <<- data.table %>% # Data table to run through the function
    mutate(DistanceX = XREF2 - X, # X axis distance to the entrance
           DistanceY = max(Y) - Y, # Y axis distance to the entrance
           PythagDist = sqrt((DistanceX^2) + (DistanceY^2)), # Shortest distance to the nest entrance 
           ScaledDist = PythagDist / MaxDist, # Scaling the distances by the max possible distance
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>% # If any distance is greater than 1 (precautionary), then it's assigned a 1
    ungroup() %>% # Ungroup the data set
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the circle nest entrance function for the Colony11CircleAggn data set
DistanceCoordsFunctionTrackerCircleAggn11(Colony11CircleAggn)

# BASELINE
DistanceCoordsFunctionTrackerCircleBase11 <- function(data.table){
  CircleCoords <- AggnStudyCircleRefCoords %>% # Reference coordinates for the circle nest
    filter(Colony == "11" & Trial == "Pre" & Nest == "Circle") %>% # Filtering out the baseline assay references
    dplyr::select(c("Coord", "XREF", "YREF")) # Select only the desired columns
  
  # X axis references
  XCoords <- CircleCoords %>% # Creating a x axis reference data set
    dplyr::select(-c("YREF")) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  
  # Y axis references
  YCoords <- CircleCoords %>% # Creating a y axis reference data set
    dplyr::select(-c("XREF")) # Remove x axis references
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # X axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # X axis reference from column 2 from above
  MaxDist = abs(YREF1 - YREF2) # Distance from the back of the nest to the entrance
  
  Colony11CirclePreDist <<- data.table %>% # Data table to run through the function
    mutate(DistanceX = XREF2 - X, # X axis distance to the entrance
           DistanceY = max(Y) - Y, # Y axis distance to the entrance
           PythagDist = sqrt((DistanceX^2) + (DistanceY^2)), # Shortest distance to the nest entrance 
           ScaledDist = PythagDist / MaxDist, # Scaling the distances by the max possible distance
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>% # If any distance is greater than 1 (precautionary), then it's assigned a 1
    ungroup() %>% # Ungroup the data set
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the circle nest entrance function for the Colony11CirclePre data set
DistanceCoordsFunctionTrackerCircleBase11(Colony11CirclePre)

# COLONY 13
# TUBE NEST
# INVADER
DistanceCoordsFunctionTrackerTubeAggn13 <- function(data.table){
  TubeCoords <- AggnStudyRefCoords %>% # Reference data set for eight even area bins
    filter(Colony == "13" & Trial == "Aggn" & Nest == "Tube") %>% # Filter out only tube nest aggression assay references
    dplyr::select(c(Coord, XREF, YREF)) # Select only desired columns 
  
  # X axis references
  XCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(YREF)) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF1 = as.numeric(XCoords[,1]) # X axis reference from column 1 from above
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  XREF3 = as.numeric(XCoords[,3]) # X axis reference from column 3 from above
  XREF4 = as.numeric(XCoords[,4]) # X axis reference from column 4 from above
  XREF5 = as.numeric(XCoords[,5]) # X axis reference from column 5 from above
  XREF6 = as.numeric(XCoords[,6]) # X axis reference from column 6 from above
  XREF7 = as.numeric(XCoords[,7]) # X axis reference from column 7 from above
  
  # Y axis references
  YCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(XREF)) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # Y axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # Y axis reference from column 2 from above
  YREF3 = as.numeric(YCoords[,3]) # Y axis reference from column 3 from above
  YREF4 = as.numeric(YCoords[,4]) # Y axis reference from column 4 from above
  YREF5 = as.numeric(YCoords[,5]) # Y axis reference from column 5 from above
  YREF6 = as.numeric(YCoords[,6]) # Y axis reference from column 6 from above
  YREF7 = as.numeric(YCoords[,7]) # Y axis reference from column 7 from above
  
  # Width of the tube nest
  Width = YREF2 - YREF3
  
  # Reference distances within the tube nest
  Dist1 = sqrt(((XREF1 - XREF2)^2) + ((YREF1 - YREF2)^2)) # Distance to the entrance from corner entering bin 1 from bin 2
  Dist2 = sqrt(((XREF2 - XREF3)^2) + ((YREF2 - YREF3)^2)) + Dist1 # Distance to the entrance from bin 3 entering bin 2
  Dist3 = sqrt(((XREF4 - XREF3)^2) + ((YREF3 - YREF4)^2)) + Dist2 # Distance to the entrance from bin 4, 5, 6 entering bin 3
  Dist4 = sqrt(((XREF5 - XREF4)^2) + ((YREF5 - YREF4)^2)) + Dist3 # Distance to the entrance from bin 7 entering bin 6
  Dist5 = sqrt(((XREF6 - XREF5)^2) + ((YREF6 - YREF5)^2)) + Dist4 # Distance to the entrance from bin 8 entering bin 7
  MaxDist = sqrt(((XREF6 - XREF7)^2) + ((YREF6 - YREF7)^2)) + Dist5 # Distance from the back of the nest to the entrance
  
  Colony13TubeAggnDist <<- data.table %>% # Data table to run through the function
    filter(Nest == "Tube") %>% # Filter out tube nest coordinates
    # Coordinate bin assignment
    mutate(X1 = ifelse(X >= XREF2 & X <= (XREF2 + (1.25 * Width)) & Y >= YREF3, # If the x axiss of each coordinate is in bin 1
                       "YES", "NO"), 
           X2 = ifelse(X < XREF2 & Y >= YREF3, # If the x axis of each coordinate is in bin 2
                       "YES", "NO"),
           X3 = ifelse(X <= XREF4 & Y < YREF3, # If the x axis of each coordinate is in bin 3
                       "YES", "NO"),
           X4 = ifelse(X > XREF4 & Y <= YREF5, # If the x axis of each coordinate is in bins 4, 5, or 6
                       "YES", "NO"),
           X5 = ifelse(X >= XREF6 & Y > YREF5, # If the x axis of each coordinate is in bin 7
                       "YES", "NO"),
           X6 = ifelse(X >= XREF7 & Y >= YREF7, # If the x axis of each coordinate is in bin 8
                       "YES", "NO"),
           Y1 = ifelse(X1 == "YES", "YES", "NO"), # If the coordinate qualified under X1
           Y2 = ifelse(X2 == "YES", "YES", "NO"), # If the coordinate qualified under X2
           Y3 = ifelse(X3 == "YES", "YES", "NO"), # If the coordinate qualified under X3
           Y4 = ifelse(X4 == "YES", "YES", "NO"), # If the coordinate qualified under X4
           Y5 = ifelse(X5 == "YES", "YES", "NO"), # If the coordinate qualified under X5
           Y6 = ifelse(X6 == "YES", "YES", "NO"), # If the coordinate qualified under X6
           # X axis distances
           DistanceX = ifelse(X1 == "YES", # Bin 1
                              X - XREF1,
                              ifelse(X2 == "YES", # Bin 2
                                     XREF2 - X,
                                     ifelse(X3 == "YES", # Bin 3
                                            XREF3 - X,
                                            ifelse(X4 == "YES", # Bins 4-6
                                                   X - XREF4,
                                                   ifelse(X5 == "YES", # Bin 7
                                                          X - XREF5,
                                                          ifelse(X6 == "YES", # Bin 8
                                                                 XREF6 - X,
                                                                 NA)))))), # Else NA
           # Y axis distances
           DistanceY = ifelse(Y1 == "YES", # Bin 1
                              YREF1 - Y,
                              ifelse(Y2 == "YES", # Bin 2
                                     YREF2 - Y,
                                     ifelse(Y3 == "YES" | Y4 == "YES" & X < XREF4, # Bin 3, or bin 4 with x coordinates less than the bin's x axis reference
                                            YREF3 - Y,
                                            ifelse(Y4 == "YES", # Bin 4-6
                                                   YREF4 - Y,
                                                   ifelse(Y5 == "YES", # Bin 7
                                                          Y - YREF5,
                                                          ifelse(Y6 == "YES", # Bin 8
                                                                 YREF6 - Y,
                                                                 NA)))))),
           # Shortest distances from the nest entrance
           # Only bin 1 is a direct line to the entrance, every other bin is the shortest distance to the closest bin to the entrance + reference distance for that bin to the entrance
           PythagDist = ifelse(X1 == "YES" & Y1 == "YES", # Bin 1
                               sqrt((DistanceX^2) + (DistanceY^2)),
                               ifelse(X2 == "YES" & Y2 == "YES", # Bin 2
                                      sqrt((DistanceX^2) + (DistanceY^2)) + Dist1,
                                      ifelse(X3 == "YES" & Y3 == "YES", # Bin 3
                                             sqrt((DistanceX^2) + (DistanceY^2)) + Dist2,
                                             ifelse(X4 == "YES" & Y4 == "YES", # Bin 4-6
                                                    sqrt((DistanceX^2) + (DistanceY^2)) + Dist3, 
                                                    ifelse(X5 == "YES" & Y5 == "YES", # Bin 7
                                                           sqrt((DistanceX^2) + (DistanceY^2)) + Dist4,
                                                           ifelse(X6 == "YES" & Y6 == "YES", # Bin 8
                                                                  sqrt((DistanceX^2) + (DistanceY^2)) + Dist5,
                                                                  NA)))))),
           # Scaling distances by the max possible distance
           ScaledDist = PythagDist / MaxDist,
           # If any distance is greater than 1 (precautionary), then it's assigned a 1
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>%
    ungroup() %>% # Ungroup the data
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the tube nest entrance function for the Colony13TubeAggn data set
DistanceCoordsFunctionTrackerTubeAggn13(Colony13TubeAggn)

# BASELINE
DistanceCoordsFunctionTrackerTubeBase13 <- function(data.table){
  TubeCoords <- AggnStudyRefCoords %>% # Reference data set for eight even area bins
    filter(Colony == "13" & Trial == "Pre" & Nest == "Tube") %>% # Filter out only tube nest baseline assay references
    dplyr::select(c(Coord, XREF, YREF)) # Select only desired columns 
  
  # X axis references
  XCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(YREF)) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF1 = as.numeric(XCoords[,1]) # X axis reference from column 1 from above
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  XREF3 = as.numeric(XCoords[,3]) # X axis reference from column 3 from above
  XREF4 = as.numeric(XCoords[,4]) # X axis reference from column 4 from above
  XREF5 = as.numeric(XCoords[,5]) # X axis reference from column 5 from above
  XREF6 = as.numeric(XCoords[,6]) # X axis reference from column 6 from above
  XREF7 = as.numeric(XCoords[,7]) # X axis reference from column 7 from above
  
  # Y axis references
  YCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(XREF)) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # Y axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # Y axis reference from column 2 from above
  YREF3 = as.numeric(YCoords[,3]) # Y axis reference from column 3 from above
  YREF4 = as.numeric(YCoords[,4]) # Y axis reference from column 4 from above
  YREF5 = as.numeric(YCoords[,5]) # Y axis reference from column 5 from above
  YREF6 = as.numeric(YCoords[,6]) # Y axis reference from column 6 from above
  YREF7 = as.numeric(YCoords[,7]) # Y axis reference from column 7 from above
  
  # Width of the tube nest
  Width = YREF2 - YREF3
  
  # Reference distances within the tube nest
  Dist1 = sqrt(((XREF1 - XREF2)^2) + ((YREF1 - YREF2)^2)) # Distance to the entrance from corner entering bin 1 from bin 2
  Dist2 = sqrt(((XREF2 - XREF3)^2) + ((YREF2 - YREF3)^2)) + Dist1 # Distance to the entrance from bin 3 entering bin 2
  Dist3 = sqrt(((XREF4 - XREF3)^2) + ((YREF3 - YREF4)^2)) + Dist2 # Distance to the entrance from bin 4, 5, 6 entering bin 3
  Dist4 = sqrt(((XREF5 - XREF4)^2) + ((YREF5 - YREF4)^2)) + Dist3 # Distance to the entrance from bin 7 entering bin 6
  Dist5 = sqrt(((XREF6 - XREF5)^2) + ((YREF6 - YREF5)^2)) + Dist4 # Distance to the entrance from bin 8 entering bin 7
  MaxDist = sqrt(((XREF6 - XREF7)^2) + ((YREF6 - YREF7)^2)) + Dist5 # Distance from the back of the nest to the entrance
  
  Colony13TubePreDist <<- data.table %>% # Data table to run through the function
    filter(Nest == "Tube") %>% # Filter out tube nest coordinates
    # Coordinate bin assignment
    mutate(X1 = ifelse(X >= XREF2 & X <= (XREF2 + (1.25 * Width)) & Y >= YREF3, # If the x axiss of each coordinate is in bin 1
                       "YES", "NO"), 
           X2 = ifelse(X < XREF2 & Y >= YREF3, # If the x axis of each coordinate is in bin 2
                       "YES", "NO"),
           X3 = ifelse(X <= XREF4 & Y < YREF3, # If the x axis of each coordinate is in bin 3
                       "YES", "NO"),
           X4 = ifelse(X > XREF4 & Y <= YREF5, # If the x axis of each coordinate is in bins 4, 5, or 6
                       "YES", "NO"),
           X5 = ifelse(X >= XREF6 & Y > YREF5, # If the x axis of each coordinate is in bin 7
                       "YES", "NO"),
           X6 = ifelse(X >= XREF7 & Y >= YREF7, # If the x axis of each coordinate is in bin 8
                       "YES", "NO"),
           Y1 = ifelse(X1 == "YES", "YES", "NO"), # If the coordinate qualified under X1
           Y2 = ifelse(X2 == "YES", "YES", "NO"), # If the coordinate qualified under X2
           Y3 = ifelse(X3 == "YES", "YES", "NO"), # If the coordinate qualified under X3
           Y4 = ifelse(X4 == "YES", "YES", "NO"), # If the coordinate qualified under X4
           Y5 = ifelse(X5 == "YES", "YES", "NO"), # If the coordinate qualified under X5
           Y6 = ifelse(X6 == "YES", "YES", "NO"), # If the coordinate qualified under X6
           # X axis distances
           DistanceX = ifelse(X1 == "YES", # Bin 1
                              X - XREF1,
                              ifelse(X2 == "YES", # Bin 2
                                     XREF2 - X,
                                     ifelse(X3 == "YES", # Bin 3
                                            XREF3 - X,
                                            ifelse(X4 == "YES", # Bins 4-6
                                                   X - XREF4,
                                                   ifelse(X5 == "YES", # Bin 7
                                                          X - XREF5,
                                                          ifelse(X6 == "YES", # Bin 8
                                                                 XREF6 - X,
                                                                 NA)))))), # Else NA
           # Y axis distances
           DistanceY = ifelse(Y1 == "YES", # Bin 1
                              YREF1 - Y,
                              ifelse(Y2 == "YES", # Bin 2
                                     YREF2 - Y,
                                     ifelse(Y3 == "YES" | Y4 == "YES" & X < XREF4, # Bin 3, or bin 4 with x coordinates less than the bin's x axis reference
                                            YREF3 - Y,
                                            ifelse(Y4 == "YES", # Bin 4-6
                                                   YREF4 - Y,
                                                   ifelse(Y5 == "YES", # Bin 7
                                                          Y - YREF5,
                                                          ifelse(Y6 == "YES", # Bin 8
                                                                 YREF6 - Y,
                                                                 NA)))))),
           # Shortest distances from the nest entrance
           # Only bin 1 is a direct line to the entrance, every other bin is the shortest distance to the closest bin to the entrance + reference distance for that bin to the entrance
           PythagDist = ifelse(X1 == "YES" & Y1 == "YES", # Bin 1
                               sqrt((DistanceX^2) + (DistanceY^2)),
                               ifelse(X2 == "YES" & Y2 == "YES", # Bin 2
                                      sqrt((DistanceX^2) + (DistanceY^2)) + Dist1,
                                      ifelse(X3 == "YES" & Y3 == "YES", # Bin 3
                                             sqrt((DistanceX^2) + (DistanceY^2)) + Dist2,
                                             ifelse(X4 == "YES" & Y4 == "YES", # Bin 4-6
                                                    sqrt((DistanceX^2) + (DistanceY^2)) + Dist3, 
                                                    ifelse(X5 == "YES" & Y5 == "YES", # Bin 7
                                                           sqrt((DistanceX^2) + (DistanceY^2)) + Dist4,
                                                           ifelse(X6 == "YES" & Y6 == "YES", # Bin 8
                                                                  sqrt((DistanceX^2) + (DistanceY^2)) + Dist5,
                                                                  NA)))))),
           # Scaling distances by the max possible distance
           ScaledDist = PythagDist / MaxDist,
           # If any distance is greater than 1 (precautionary), then it's assigned a 1
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>%
    ungroup() %>% # Ungroup the data
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the tube nest entrance function for the Colony13TubePre data set
DistanceCoordsFunctionTrackerTubeBase13(Colony13TubePre)

# CIRCLE NEST
# INVADER
DistanceCoordsFunctionTrackerCircleAggn13 <-function(data.table){
  CircleCoords <- AggnStudyCircleRefCoords %>% # Reference coordinates for the circle nest
    filter(Colony == "13" & Trial == "Aggn" & Nest == "Circle") %>% # Filtering out the aggression assay references
    dplyr::select(c("Coord", "XREF", "YREF")) # Select only the desired columns
  
  # X axis references
  XCoords <- CircleCoords %>% # Creating a x axis reference data set
    dplyr::select(-c("YREF")) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  
  # Y axis references
  YCoords <- CircleCoords %>% # Creating a y axis reference data set
    dplyr::select(-c("XREF")) # Remove x axis references
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # X axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # X axis reference from column 2 from above
  MaxDist = abs(YREF1 - YREF2) # Distance from the back of the nest to the entrance
  
  Colony13CircleAggnDist <<- data.table %>% # Data table to run through the function
    mutate(DistanceX = XREF2 - X, # X axis distance to the entrance
           DistanceY = max(Y) - Y, # Y axis distance to the entrance
           PythagDist = sqrt((DistanceX^2) + (DistanceY^2)), # Shortest distance to the nest entrance 
           ScaledDist = PythagDist / MaxDist, # Scaling the distances by the max possible distance
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>% # If any distance is greater than 1 (precautionary), then it's assigned a 1
    ungroup() %>% # Ungroup the data set
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the circle nest entrance function for the Colony11CircleAggn data set
DistanceCoordsFunctionTrackerCircleAggn13(Colony13CircleAggn)

# BASELINE
DistanceCoordsFunctionTrackerCircleBase13 <- function(data.table){
  CircleCoords <- AggnStudyCircleRefCoords %>% # Reference coordinates for the circle nest
    filter(Colony == "13" & Trial == "Pre" & Nest == "Circle") %>% # Filtering out the baseline assay references
    dplyr::select(c("Coord", "XREF", "YREF")) # Select only the desired columns
  
  # X axis references
  XCoords <- CircleCoords %>% # Creating a x axis reference data set
    dplyr::select(-c("YREF")) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  
  # Y axis references
  YCoords <- CircleCoords %>% # Creating a y axis reference data set
    dplyr::select(-c("XREF")) # Remove x axis references
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # X axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # X axis reference from column 2 from above
  MaxDist = abs(YREF1 - YREF2) # Distance from the back of the nest to the entrance
  
  Colony13CirclePreDist <<- data.table %>% # Data table to run through the function
    mutate(DistanceX = XREF2 - X, # X axis distance to the entrance
           DistanceY = max(Y) - Y, # Y axis distance to the entrance
           PythagDist = sqrt((DistanceX^2) + (DistanceY^2)), # Shortest distance to the nest entrance 
           ScaledDist = PythagDist / MaxDist, # Scaling the distances by the max possible distance
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>% # If any distance is greater than 1 (precautionary), then it's assigned a 1
    ungroup() %>% # Ungroup the data set
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the circle nest entrance function for the Colony13CirclePre data set
DistanceCoordsFunctionTrackerCircleBase13(Colony13CirclePre)

# COLONY 17
# TUBE NEST
# INVADER
DistanceCoordsFunctionTrackerTubeAggn17 <- function(data.table){
  TubeCoords <- AggnStudyRefCoords %>% # Reference data set for eight even area bins
    filter(Colony == "17" & Trial == "Aggn" & Nest == "Tube") %>% # Filter out only tube nest aggression assay references
    dplyr::select(c(Coord, XREF, YREF)) # Select only desired columns 
  
  # X axis references
  XCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(YREF)) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF1 = as.numeric(XCoords[,1]) # X axis reference from column 1 from above
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  XREF3 = as.numeric(XCoords[,3]) # X axis reference from column 3 from above
  XREF4 = as.numeric(XCoords[,4]) # X axis reference from column 4 from above
  XREF5 = as.numeric(XCoords[,5]) # X axis reference from column 5 from above
  XREF6 = as.numeric(XCoords[,6]) # X axis reference from column 6 from above
  XREF7 = as.numeric(XCoords[,7]) # X axis reference from column 7 from above
  
  # Y axis references
  YCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(XREF)) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # Y axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # Y axis reference from column 2 from above
  YREF3 = as.numeric(YCoords[,3]) # Y axis reference from column 3 from above
  YREF4 = as.numeric(YCoords[,4]) # Y axis reference from column 4 from above
  YREF5 = as.numeric(YCoords[,5]) # Y axis reference from column 5 from above
  YREF6 = as.numeric(YCoords[,6]) # Y axis reference from column 6 from above
  YREF7 = as.numeric(YCoords[,7]) # Y axis reference from column 7 from above
  
  # Width of the tube nest
  Width = YREF2 - YREF3
  
  # Reference distances within the tube nest
  Dist1 = sqrt(((XREF1 - XREF2)^2) + ((YREF1 - YREF2)^2)) # Distance to the entrance from corner entering bin 1 from bin 2
  Dist2 = sqrt(((XREF2 - XREF3)^2) + ((YREF2 - YREF3)^2)) + Dist1 # Distance to the entrance from bin 3 entering bin 2
  Dist3 = sqrt(((XREF4 - XREF3)^2) + ((YREF3 - YREF4)^2)) + Dist2 # Distance to the entrance from bin 4, 5, 6 entering bin 3
  Dist4 = sqrt(((XREF5 - XREF4)^2) + ((YREF5 - YREF4)^2)) + Dist3 # Distance to the entrance from bin 7 entering bin 6
  Dist5 = sqrt(((XREF6 - XREF5)^2) + ((YREF6 - YREF5)^2)) + Dist4 # Distance to the entrance from bin 8 entering bin 7
  MaxDist = sqrt(((XREF6 - XREF7)^2) + ((YREF6 - YREF7)^2)) + Dist5 # Distance from the back of the nest to the entrance
  
  Colony17TubeAggnDist <<- data.table %>% # Data table to run through the function
    filter(Nest == "Tube") %>% # Filter out tube nest coordinates
    # Coordinate bin assignment
    mutate(X1 = ifelse(X >= XREF2 & X <= (XREF2 + (1.25 * Width)) & Y >= YREF3, # If the x axiss of each coordinate is in bin 1
                       "YES", "NO"), 
           X2 = ifelse(X < XREF2 & Y >= YREF3, # If the x axis of each coordinate is in bin 2
                       "YES", "NO"),
           X3 = ifelse(X <= XREF4 & Y < YREF3, # If the x axis of each coordinate is in bin 3
                       "YES", "NO"),
           X4 = ifelse(X > XREF4 & Y <= YREF5, # If the x axis of each coordinate is in bins 4, 5, or 6
                       "YES", "NO"),
           X5 = ifelse(X >= XREF6 & Y > YREF5, # If the x axis of each coordinate is in bin 7
                       "YES", "NO"),
           X6 = ifelse(X >= XREF7 & Y >= YREF7, # If the x axis of each coordinate is in bin 8
                       "YES", "NO"),
           Y1 = ifelse(X1 == "YES", "YES", "NO"), # If the coordinate qualified under X1
           Y2 = ifelse(X2 == "YES", "YES", "NO"), # If the coordinate qualified under X2
           Y3 = ifelse(X3 == "YES", "YES", "NO"), # If the coordinate qualified under X3
           Y4 = ifelse(X4 == "YES", "YES", "NO"), # If the coordinate qualified under X4
           Y5 = ifelse(X5 == "YES", "YES", "NO"), # If the coordinate qualified under X5
           Y6 = ifelse(X6 == "YES", "YES", "NO"), # If the coordinate qualified under X6
           # X axis distances
           DistanceX = ifelse(X1 == "YES", # Bin 1
                              X - XREF1,
                              ifelse(X2 == "YES", # Bin 2
                                     XREF2 - X,
                                     ifelse(X3 == "YES", # Bin 3
                                            XREF3 - X,
                                            ifelse(X4 == "YES", # Bins 4-6
                                                   X - XREF4,
                                                   ifelse(X5 == "YES", # Bin 7
                                                          X - XREF5,
                                                          ifelse(X6 == "YES", # Bin 8
                                                                 XREF6 - X,
                                                                 NA)))))), # Else NA
           # Y axis distances
           DistanceY = ifelse(Y1 == "YES", # Bin 1
                              YREF1 - Y,
                              ifelse(Y2 == "YES", # Bin 2
                                     YREF2 - Y,
                                     ifelse(Y3 == "YES" | Y4 == "YES" & X < XREF4, # Bin 3, or bin 4 with x coordinates less than the bin's x axis reference
                                            YREF3 - Y,
                                            ifelse(Y4 == "YES", # Bin 4-6
                                                   YREF4 - Y,
                                                   ifelse(Y5 == "YES", # Bin 7
                                                          Y - YREF5,
                                                          ifelse(Y6 == "YES", # Bin 8
                                                                 YREF6 - Y,
                                                                 NA)))))),
           # Shortest distances from the nest entrance
           # Only bin 1 is a direct line to the entrance, every other bin is the shortest distance to the closest bin to the entrance + reference distance for that bin to the entrance
           PythagDist = ifelse(X1 == "YES" & Y1 == "YES", # Bin 1
                               sqrt((DistanceX^2) + (DistanceY^2)),
                               ifelse(X2 == "YES" & Y2 == "YES", # Bin 2
                                      sqrt((DistanceX^2) + (DistanceY^2)) + Dist1,
                                      ifelse(X3 == "YES" & Y3 == "YES", # Bin 3
                                             sqrt((DistanceX^2) + (DistanceY^2)) + Dist2,
                                             ifelse(X4 == "YES" & Y4 == "YES", # Bin 4-6
                                                    sqrt((DistanceX^2) + (DistanceY^2)) + Dist3, 
                                                    ifelse(X5 == "YES" & Y5 == "YES", # Bin 7
                                                           sqrt((DistanceX^2) + (DistanceY^2)) + Dist4,
                                                           ifelse(X6 == "YES" & Y6 == "YES", # Bin 8
                                                                  sqrt((DistanceX^2) + (DistanceY^2)) + Dist5,
                                                                  NA)))))),
           # Scaling distances by the max possible distance
           ScaledDist = PythagDist / MaxDist,
           # If any distance is greater than 1 (precautionary), then it's assigned a 1
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>%
    ungroup() %>% # Ungroup the data
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the tube nest entrance function for the Colony17TubeAggn data set
DistanceCoordsFunctionTrackerTubeAggn17(Colony17TubeAggn)

# BASELINE
DistanceCoordsFunctionTrackerTubeBase17 <- function(data.table){
  TubeCoords <- AggnStudyRefCoords %>% # Reference data set for eight even area bins
    filter(Colony == "17" & Trial == "Pre" & Nest == "Tube") %>% # Filter out only tube nest baseline assay references
    dplyr::select(c(Coord, XREF, YREF)) # Select only desired columns 
  
  # X axis references
  XCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(YREF)) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF1 = as.numeric(XCoords[,1]) # X axis reference from column 1 from above
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  XREF3 = as.numeric(XCoords[,3]) # X axis reference from column 3 from above
  XREF4 = as.numeric(XCoords[,4]) # X axis reference from column 4 from above
  XREF5 = as.numeric(XCoords[,5]) # X axis reference from column 5 from above
  XREF6 = as.numeric(XCoords[,6]) # X axis reference from column 6 from above
  XREF7 = as.numeric(XCoords[,7]) # X axis reference from column 7 from above
  
  # Y axis references
  YCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(XREF)) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # Y axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # Y axis reference from column 2 from above
  YREF3 = as.numeric(YCoords[,3]) # Y axis reference from column 3 from above
  YREF4 = as.numeric(YCoords[,4]) # Y axis reference from column 4 from above
  YREF5 = as.numeric(YCoords[,5]) # Y axis reference from column 5 from above
  YREF6 = as.numeric(YCoords[,6]) # Y axis reference from column 6 from above
  YREF7 = as.numeric(YCoords[,7]) # Y axis reference from column 7 from above
  
  # Width of the tube nest
  Width = YREF2 - YREF3
  
  # Reference distances within the tube nest
  Dist1 = sqrt(((XREF1 - XREF2)^2) + ((YREF1 - YREF2)^2)) # Distance to the entrance from corner entering bin 1 from bin 2
  Dist2 = sqrt(((XREF2 - XREF3)^2) + ((YREF2 - YREF3)^2)) + Dist1 # Distance to the entrance from bin 3 entering bin 2
  Dist3 = sqrt(((XREF4 - XREF3)^2) + ((YREF3 - YREF4)^2)) + Dist2 # Distance to the entrance from bin 4, 5, 6 entering bin 3
  Dist4 = sqrt(((XREF5 - XREF4)^2) + ((YREF5 - YREF4)^2)) + Dist3 # Distance to the entrance from bin 7 entering bin 6
  Dist5 = sqrt(((XREF6 - XREF5)^2) + ((YREF6 - YREF5)^2)) + Dist4 # Distance to the entrance from bin 8 entering bin 7
  MaxDist = sqrt(((XREF6 - XREF7)^2) + ((YREF6 - YREF7)^2)) + Dist5 # Distance from the back of the nest to the entrance
  
  Colony17TubePreDist <<- data.table %>% # Data table to run through the function
    filter(Nest == "Tube") %>% # Filter out tube nest coordinates
    # Coordinate bin assignment
    mutate(X1 = ifelse(X >= XREF2 & X <= (XREF2 + (1.25 * Width)) & Y >= YREF3, # If the x axiss of each coordinate is in bin 1
                       "YES", "NO"), 
           X2 = ifelse(X < XREF2 & Y >= YREF3, # If the x axis of each coordinate is in bin 2
                       "YES", "NO"),
           X3 = ifelse(X <= XREF4 & Y < YREF3, # If the x axis of each coordinate is in bin 3
                       "YES", "NO"),
           X4 = ifelse(X > XREF4 & Y <= YREF5, # If the x axis of each coordinate is in bins 4, 5, or 6
                       "YES", "NO"),
           X5 = ifelse(X >= XREF6 & Y > YREF5, # If the x axis of each coordinate is in bin 7
                       "YES", "NO"),
           X6 = ifelse(X >= XREF7 & Y >= YREF7, # If the x axis of each coordinate is in bin 8
                       "YES", "NO"),
           Y1 = ifelse(X1 == "YES", "YES", "NO"), # If the coordinate qualified under X1
           Y2 = ifelse(X2 == "YES", "YES", "NO"), # If the coordinate qualified under X2
           Y3 = ifelse(X3 == "YES", "YES", "NO"), # If the coordinate qualified under X3
           Y4 = ifelse(X4 == "YES", "YES", "NO"), # If the coordinate qualified under X4
           Y5 = ifelse(X5 == "YES", "YES", "NO"), # If the coordinate qualified under X5
           Y6 = ifelse(X6 == "YES", "YES", "NO"), # If the coordinate qualified under X6
           # X axis distances
           DistanceX = ifelse(X1 == "YES", # Bin 1
                              X - XREF1,
                              ifelse(X2 == "YES", # Bin 2
                                     XREF2 - X,
                                     ifelse(X3 == "YES", # Bin 3
                                            XREF3 - X,
                                            ifelse(X4 == "YES", # Bins 4-6
                                                   X - XREF4,
                                                   ifelse(X5 == "YES", # Bin 7
                                                          X - XREF5,
                                                          ifelse(X6 == "YES", # Bin 8
                                                                 XREF6 - X,
                                                                 NA)))))), # Else NA
           # Y axis distances
           DistanceY = ifelse(Y1 == "YES", # Bin 1
                              YREF1 - Y,
                              ifelse(Y2 == "YES", # Bin 2
                                     YREF2 - Y,
                                     ifelse(Y3 == "YES" | Y4 == "YES" & X < XREF4, # Bin 3, or bin 4 with x coordinates less than the bin's x axis reference
                                            YREF3 - Y,
                                            ifelse(Y4 == "YES", # Bin 4-6
                                                   YREF4 - Y,
                                                   ifelse(Y5 == "YES", # Bin 7
                                                          Y - YREF5,
                                                          ifelse(Y6 == "YES", # Bin 8
                                                                 YREF6 - Y,
                                                                 NA)))))),
           # Shortest distances from the nest entrance
           # Only bin 1 is a direct line to the entrance, every other bin is the shortest distance to the closest bin to the entrance + reference distance for that bin to the entrance
           PythagDist = ifelse(X1 == "YES" & Y1 == "YES", # Bin 1
                               sqrt((DistanceX^2) + (DistanceY^2)),
                               ifelse(X2 == "YES" & Y2 == "YES", # Bin 2
                                      sqrt((DistanceX^2) + (DistanceY^2)) + Dist1,
                                      ifelse(X3 == "YES" & Y3 == "YES", # Bin 3
                                             sqrt((DistanceX^2) + (DistanceY^2)) + Dist2,
                                             ifelse(X4 == "YES" & Y4 == "YES", # Bin 4-6
                                                    sqrt((DistanceX^2) + (DistanceY^2)) + Dist3, 
                                                    ifelse(X5 == "YES" & Y5 == "YES", # Bin 7
                                                           sqrt((DistanceX^2) + (DistanceY^2)) + Dist4,
                                                           ifelse(X6 == "YES" & Y6 == "YES", # Bin 8
                                                                  sqrt((DistanceX^2) + (DistanceY^2)) + Dist5,
                                                                  NA)))))),
           # Scaling distances by the max possible distance
           ScaledDist = PythagDist / MaxDist,
           # If any distance is greater than 1 (precautionary), then it's assigned a 1
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>%
    ungroup() %>% # Ungroup the data
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the tube nest entrance function for the Colony17TubePre data set
DistanceCoordsFunctionTrackerTubeBase17(Colony17TubePre)

# CIRCLE NEST
# INVADER
DistanceCoordsFunctionTrackerCircleAggn17 <-function(data.table){
  CircleCoords <- AggnStudyCircleRefCoords %>% # Reference coordinates for the circle nest
    filter(Colony == "17" & Trial == "Aggn" & Nest == "Circle") %>% # Filtering out the aggression assay references
    dplyr::select(c("Coord", "XREF", "YREF")) # Select only the desired columns
  
  # X axis references
  XCoords <- CircleCoords %>% # Creating a x axis reference data set
    dplyr::select(-c("YREF")) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  
  # Y axis references
  YCoords <- CircleCoords %>% # Creating a y axis reference data set
    dplyr::select(-c("XREF")) # Remove x axis references
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # X axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # X axis reference from column 2 from above
  MaxDist = abs(YREF1 - YREF2) # Distance from the back of the nest to the entrance
  
  Colony17CircleAggnDist <<- data.table %>% # Data table to run through the function
    mutate(DistanceX = XREF2 - X, # X axis distance to the entrance
           DistanceY = max(Y) - Y, # Y axis distance to the entrance
           PythagDist = sqrt((DistanceX^2) + (DistanceY^2)), # Shortest distance to the nest entrance 
           ScaledDist = PythagDist / MaxDist, # Scaling the distances by the max possible distance
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>% # If any distance is greater than 1 (precautionary), then it's assigned a 1
    ungroup() %>% # Ungroup the data set
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the circle nest entrance function for the Colony11CircleAggn data set
DistanceCoordsFunctionTrackerCircleAggn17(Colony17CircleAggn)

# BASELINE
DistanceCoordsFunctionTrackerCircleBase17 <- function(data.table){
  CircleCoords <- AggnStudyCircleRefCoords %>% # Reference coordinates for the circle nest
    filter(Colony == "17" & Trial == "Pre" & Nest == "Circle") %>% # Filtering out the baseline assay references
    dplyr::select(c("Coord", "XREF", "YREF")) # Select only the desired columns
  
  # X axis references
  XCoords <- CircleCoords %>% # Creating a x axis reference data set
    dplyr::select(-c("YREF")) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  
  # Y axis references
  YCoords <- CircleCoords %>% # Creating a y axis reference data set
    dplyr::select(-c("XREF")) # Remove x axis references
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # X axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # X axis reference from column 2 from above
  MaxDist = abs(YREF1 - YREF2) # Distance from the back of the nest to the entrance
  
  Colony17CirclePreDist <<- data.table %>% # Data table to run through the function
    mutate(DistanceX = XREF2 - X, # X axis distance to the entrance
           DistanceY = max(Y) - Y, # Y axis distance to the entrance
           PythagDist = sqrt((DistanceX^2) + (DistanceY^2)), # Shortest distance to the nest entrance 
           ScaledDist = PythagDist / MaxDist, # Scaling the distances by the max possible distance
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>% # If any distance is greater than 1 (precautionary), then it's assigned a 1
    ungroup() %>% # Ungroup the data set
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the circle nest entrance function for the Colony17CirclePre data set
DistanceCoordsFunctionTrackerCircleBase17(Colony17CirclePre)

# COLONY 18
# TUBE NEST
# INVADER
DistanceCoordsFunctionTrackerTubeAggn18 <- function(data.table){
  TubeCoords <- AggnStudyRefCoords %>% # Reference data set for eight even area bins
    filter(Colony == "18" & Trial == "Aggn" & Nest == "Tube") %>% # Filter out only tube nest aggression assay references
    dplyr::select(c(Coord, XREF, YREF)) # Select only desired columns 
  
  # X axis references
  XCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(YREF)) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF1 = as.numeric(XCoords[,1]) # X axis reference from column 1 from above
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  XREF3 = as.numeric(XCoords[,3]) # X axis reference from column 3 from above
  XREF4 = as.numeric(XCoords[,4]) # X axis reference from column 4 from above
  XREF5 = as.numeric(XCoords[,5]) # X axis reference from column 5 from above
  XREF6 = as.numeric(XCoords[,6]) # X axis reference from column 6 from above
  XREF7 = as.numeric(XCoords[,7]) # X axis reference from column 7 from above
  
  # Y axis references
  YCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(XREF)) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # Y axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # Y axis reference from column 2 from above
  YREF3 = as.numeric(YCoords[,3]) # Y axis reference from column 3 from above
  YREF4 = as.numeric(YCoords[,4]) # Y axis reference from column 4 from above
  YREF5 = as.numeric(YCoords[,5]) # Y axis reference from column 5 from above
  YREF6 = as.numeric(YCoords[,6]) # Y axis reference from column 6 from above
  YREF7 = as.numeric(YCoords[,7]) # Y axis reference from column 7 from above
  
  # Width of the tube nest
  Width = YREF2 - YREF3
  
  # Reference distances within the tube nest
  Dist1 = sqrt(((XREF1 - XREF2)^2) + ((YREF1 - YREF2)^2)) # Distance to the entrance from corner entering bin 1 from bin 2
  Dist2 = sqrt(((XREF2 - XREF3)^2) + ((YREF2 - YREF3)^2)) + Dist1 # Distance to the entrance from bin 3 entering bin 2
  Dist3 = sqrt(((XREF4 - XREF3)^2) + ((YREF3 - YREF4)^2)) + Dist2 # Distance to the entrance from bin 4, 5, 6 entering bin 3
  Dist4 = sqrt(((XREF5 - XREF4)^2) + ((YREF5 - YREF4)^2)) + Dist3 # Distance to the entrance from bin 7 entering bin 6
  Dist5 = sqrt(((XREF6 - XREF5)^2) + ((YREF6 - YREF5)^2)) + Dist4 # Distance to the entrance from bin 8 entering bin 7
  MaxDist = sqrt(((XREF6 - XREF7)^2) + ((YREF6 - YREF7)^2)) + Dist5 # Distance from the back of the nest to the entrance
  
  Colony18TubeAggnDist <<- data.table %>% # Data table to run through the function
    filter(Nest == "Tube") %>% # Filter out tube nest coordinates
    # Coordinate bin assignment
    mutate(X1 = ifelse(X >= XREF2 & X <= (XREF2 + (1.25 * Width)) & Y >= YREF3, # If the x axiss of each coordinate is in bin 1
                       "YES", "NO"), 
           X2 = ifelse(X < XREF2 & Y >= YREF3, # If the x axis of each coordinate is in bin 2
                       "YES", "NO"),
           X3 = ifelse(X <= XREF4 & Y < YREF3, # If the x axis of each coordinate is in bin 3
                       "YES", "NO"),
           X4 = ifelse(X > XREF4 & Y <= YREF5, # If the x axis of each coordinate is in bins 4, 5, or 6
                       "YES", "NO"),
           X5 = ifelse(X >= XREF6 & Y > YREF5, # If the x axis of each coordinate is in bin 7
                       "YES", "NO"),
           X6 = ifelse(X >= XREF7 & Y >= YREF7, # If the x axis of each coordinate is in bin 8
                       "YES", "NO"),
           Y1 = ifelse(X1 == "YES", "YES", "NO"), # If the coordinate qualified under X1
           Y2 = ifelse(X2 == "YES", "YES", "NO"), # If the coordinate qualified under X2
           Y3 = ifelse(X3 == "YES", "YES", "NO"), # If the coordinate qualified under X3
           Y4 = ifelse(X4 == "YES", "YES", "NO"), # If the coordinate qualified under X4
           Y5 = ifelse(X5 == "YES", "YES", "NO"), # If the coordinate qualified under X5
           Y6 = ifelse(X6 == "YES", "YES", "NO"), # If the coordinate qualified under X6
           # X axis distances
           DistanceX = ifelse(X1 == "YES", # Bin 1
                              X - XREF1,
                              ifelse(X2 == "YES", # Bin 2
                                     XREF2 - X,
                                     ifelse(X3 == "YES", # Bin 3
                                            XREF3 - X,
                                            ifelse(X4 == "YES", # Bins 4-6
                                                   X - XREF4,
                                                   ifelse(X5 == "YES", # Bin 7
                                                          X - XREF5,
                                                          ifelse(X6 == "YES", # Bin 8
                                                                 XREF6 - X,
                                                                 NA)))))), # Else NA
           # Y axis distances
           DistanceY = ifelse(Y1 == "YES", # Bin 1
                              YREF1 - Y,
                              ifelse(Y2 == "YES", # Bin 2
                                     YREF2 - Y,
                                     ifelse(Y3 == "YES" | Y4 == "YES" & X < XREF4, # Bin 3, or bin 4 with x coordinates less than the bin's x axis reference
                                            YREF3 - Y,
                                            ifelse(Y4 == "YES", # Bin 4-6
                                                   YREF4 - Y,
                                                   ifelse(Y5 == "YES", # Bin 7
                                                          Y - YREF5,
                                                          ifelse(Y6 == "YES", # Bin 8
                                                                 YREF6 - Y,
                                                                 NA)))))),
           # Shortest distances from the nest entrance
           # Only bin 1 is a direct line to the entrance, every other bin is the shortest distance to the closest bin to the entrance + reference distance for that bin to the entrance
           PythagDist = ifelse(X1 == "YES" & Y1 == "YES", # Bin 1
                               sqrt((DistanceX^2) + (DistanceY^2)),
                               ifelse(X2 == "YES" & Y2 == "YES", # Bin 2
                                      sqrt((DistanceX^2) + (DistanceY^2)) + Dist1,
                                      ifelse(X3 == "YES" & Y3 == "YES", # Bin 3
                                             sqrt((DistanceX^2) + (DistanceY^2)) + Dist2,
                                             ifelse(X4 == "YES" & Y4 == "YES", # Bin 4-6
                                                    sqrt((DistanceX^2) + (DistanceY^2)) + Dist3, 
                                                    ifelse(X5 == "YES" & Y5 == "YES", # Bin 7
                                                           sqrt((DistanceX^2) + (DistanceY^2)) + Dist4,
                                                           ifelse(X6 == "YES" & Y6 == "YES", # Bin 8
                                                                  sqrt((DistanceX^2) + (DistanceY^2)) + Dist5,
                                                                  NA)))))),
           # Scaling distances by the max possible distance
           ScaledDist = PythagDist / MaxDist,
           # If any distance is greater than 1 (precautionary), then it's assigned a 1
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>%
    ungroup() %>% # Ungroup the data
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the tube nest entrance function for the Colony18TubeAggn data set
DistanceCoordsFunctionTrackerTubeAggn18(Colony18TubeAggn)

# BASELINE
DistanceCoordsFunctionTrackerTubeBase18 <- function(data.table){
  TubeCoords <- AggnStudyRefCoords %>% # Reference data set for eight even area bins
    filter(Colony == "18" & Trial == "Pre" & Nest == "Tube") %>% # Filter out only tube nest baseline assay references
    dplyr::select(c(Coord, XREF, YREF)) # Select only desired columns 
  
  # X axis references
  XCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(YREF)) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF1 = as.numeric(XCoords[,1]) # X axis reference from column 1 from above
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  XREF3 = as.numeric(XCoords[,3]) # X axis reference from column 3 from above
  XREF4 = as.numeric(XCoords[,4]) # X axis reference from column 4 from above
  XREF5 = as.numeric(XCoords[,5]) # X axis reference from column 5 from above
  XREF6 = as.numeric(XCoords[,6]) # X axis reference from column 6 from above
  XREF7 = as.numeric(XCoords[,7]) # X axis reference from column 7 from above
  
  # Y axis references
  YCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(XREF)) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # Y axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # Y axis reference from column 2 from above
  YREF3 = as.numeric(YCoords[,3]) # Y axis reference from column 3 from above
  YREF4 = as.numeric(YCoords[,4]) # Y axis reference from column 4 from above
  YREF5 = as.numeric(YCoords[,5]) # Y axis reference from column 5 from above
  YREF6 = as.numeric(YCoords[,6]) # Y axis reference from column 6 from above
  YREF7 = as.numeric(YCoords[,7]) # Y axis reference from column 7 from above
  
  # Width of the tube nest
  Width = YREF2 - YREF3
  
  # Reference distances within the tube nest
  Dist1 = sqrt(((XREF1 - XREF2)^2) + ((YREF1 - YREF2)^2)) # Distance to the entrance from corner entering bin 1 from bin 2
  Dist2 = sqrt(((XREF2 - XREF3)^2) + ((YREF2 - YREF3)^2)) + Dist1 # Distance to the entrance from bin 3 entering bin 2
  Dist3 = sqrt(((XREF4 - XREF3)^2) + ((YREF3 - YREF4)^2)) + Dist2 # Distance to the entrance from bin 4, 5, 6 entering bin 3
  Dist4 = sqrt(((XREF5 - XREF4)^2) + ((YREF5 - YREF4)^2)) + Dist3 # Distance to the entrance from bin 7 entering bin 6
  Dist5 = sqrt(((XREF6 - XREF5)^2) + ((YREF6 - YREF5)^2)) + Dist4 # Distance to the entrance from bin 8 entering bin 7
  MaxDist = sqrt(((XREF6 - XREF7)^2) + ((YREF6 - YREF7)^2)) + Dist5 # Distance from the back of the nest to the entrance
  
  Colony18TubePreDist <<- data.table %>% # Data table to run through the function
    filter(Nest == "Tube") %>% # Filter out tube nest coordinates
    # Coordinate bin assignment
    mutate(X1 = ifelse(X >= XREF2 & X <= (XREF2 + (1.25 * Width)) & Y >= YREF3, # If the x axiss of each coordinate is in bin 1
                       "YES", "NO"), 
           X2 = ifelse(X < XREF2 & Y >= YREF3, # If the x axis of each coordinate is in bin 2
                       "YES", "NO"),
           X3 = ifelse(X <= XREF4 & Y < YREF3, # If the x axis of each coordinate is in bin 3
                       "YES", "NO"),
           X4 = ifelse(X > XREF4 & Y <= YREF5, # If the x axis of each coordinate is in bins 4, 5, or 6
                       "YES", "NO"),
           X5 = ifelse(X >= XREF6 & Y > YREF5, # If the x axis of each coordinate is in bin 7
                       "YES", "NO"),
           X6 = ifelse(X >= XREF7 & Y >= YREF7, # If the x axis of each coordinate is in bin 8
                       "YES", "NO"),
           Y1 = ifelse(X1 == "YES", "YES", "NO"), # If the coordinate qualified under X1
           Y2 = ifelse(X2 == "YES", "YES", "NO"), # If the coordinate qualified under X2
           Y3 = ifelse(X3 == "YES", "YES", "NO"), # If the coordinate qualified under X3
           Y4 = ifelse(X4 == "YES", "YES", "NO"), # If the coordinate qualified under X4
           Y5 = ifelse(X5 == "YES", "YES", "NO"), # If the coordinate qualified under X5
           Y6 = ifelse(X6 == "YES", "YES", "NO"), # If the coordinate qualified under X6
           # X axis distances
           DistanceX = ifelse(X1 == "YES", # Bin 1
                              X - XREF1,
                              ifelse(X2 == "YES", # Bin 2
                                     XREF2 - X,
                                     ifelse(X3 == "YES", # Bin 3
                                            XREF3 - X,
                                            ifelse(X4 == "YES", # Bins 4-6
                                                   X - XREF4,
                                                   ifelse(X5 == "YES", # Bin 7
                                                          X - XREF5,
                                                          ifelse(X6 == "YES", # Bin 8
                                                                 XREF6 - X,
                                                                 NA)))))), # Else NA
           # Y axis distances
           DistanceY = ifelse(Y1 == "YES", # Bin 1
                              YREF1 - Y,
                              ifelse(Y2 == "YES", # Bin 2
                                     YREF2 - Y,
                                     ifelse(Y3 == "YES" | Y4 == "YES" & X < XREF4, # Bin 3, or bin 4 with x coordinates less than the bin's x axis reference
                                            YREF3 - Y,
                                            ifelse(Y4 == "YES", # Bin 4-6
                                                   YREF4 - Y,
                                                   ifelse(Y5 == "YES", # Bin 7
                                                          Y - YREF5,
                                                          ifelse(Y6 == "YES", # Bin 8
                                                                 YREF6 - Y,
                                                                 NA)))))),
           # Shortest distances from the nest entrance
           # Only bin 1 is a direct line to the entrance, every other bin is the shortest distance to the closest bin to the entrance + reference distance for that bin to the entrance
           PythagDist = ifelse(X1 == "YES" & Y1 == "YES", # Bin 1
                               sqrt((DistanceX^2) + (DistanceY^2)),
                               ifelse(X2 == "YES" & Y2 == "YES", # Bin 2
                                      sqrt((DistanceX^2) + (DistanceY^2)) + Dist1,
                                      ifelse(X3 == "YES" & Y3 == "YES", # Bin 3
                                             sqrt((DistanceX^2) + (DistanceY^2)) + Dist2,
                                             ifelse(X4 == "YES" & Y4 == "YES", # Bin 4-6
                                                    sqrt((DistanceX^2) + (DistanceY^2)) + Dist3, 
                                                    ifelse(X5 == "YES" & Y5 == "YES", # Bin 7
                                                           sqrt((DistanceX^2) + (DistanceY^2)) + Dist4,
                                                           ifelse(X6 == "YES" & Y6 == "YES", # Bin 8
                                                                  sqrt((DistanceX^2) + (DistanceY^2)) + Dist5,
                                                                  NA)))))),
           # Scaling distances by the max possible distance
           ScaledDist = PythagDist / MaxDist,
           # If any distance is greater than 1 (precautionary), then it's assigned a 1
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>%
    ungroup() %>% # Ungroup the data
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the tube nest entrance function for the Colony18TubePre data set
DistanceCoordsFunctionTrackerTubeBase18(Colony18TubePre)

# CIRCLE NEST
# INVADER
DistanceCoordsFunctionTrackerCircleAggn18 <-function(data.table){
  CircleCoords <- AggnStudyCircleRefCoords %>% # Reference coordinates for the circle nest
    filter(Colony == "18" & Trial == "Aggn" & Nest == "Circle") %>% # Filtering out the aggression assay references
    dplyr::select(c("Coord", "XREF", "YREF")) # Select only the desired columns
  
  # X axis references
  XCoords <- CircleCoords %>% # Creating a x axis reference data set
    dplyr::select(-c("YREF")) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  
  # Y axis references
  YCoords <- CircleCoords %>% # Creating a y axis reference data set
    dplyr::select(-c("XREF")) # Remove x axis references
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # X axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # X axis reference from column 2 from above
  MaxDist = abs(YREF1 - YREF2) # Distance from the back of the nest to the entrance
  
  Colony18CircleAggnDist <<- data.table %>% # Data table to run through the function
    mutate(DistanceX = XREF2 - X, # X axis distance to the entrance
           DistanceY = max(Y) - Y, # Y axis distance to the entrance
           PythagDist = sqrt((DistanceX^2) + (DistanceY^2)), # Shortest distance to the nest entrance 
           ScaledDist = PythagDist / MaxDist, # Scaling the distances by the max possible distance
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>% # If any distance is greater than 1 (precautionary), then it's assigned a 1
    ungroup() %>% # Ungroup the data set
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the circle nest entrance function for the Colony11CircleAggn data set
DistanceCoordsFunctionTrackerCircleAggn18(Colony18CircleAggn)

# BASELINE
DistanceCoordsFunctionTrackerCircleBase18 <- function(data.table){
  CircleCoords <- AggnStudyCircleRefCoords %>% # Reference coordinates for the circle nest
    filter(Colony == "18" & Trial == "Pre" & Nest == "Circle") %>% # Filtering out the baseline assay references
    dplyr::select(c("Coord", "XREF", "YREF")) # Select only the desired columns
  
  # X axis references
  XCoords <- CircleCoords %>% # Creating a x axis reference data set
    dplyr::select(-c("YREF")) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  
  # Y axis references
  YCoords <- CircleCoords %>% # Creating a y axis reference data set
    dplyr::select(-c("XREF")) # Remove x axis references
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # X axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # X axis reference from column 2 from above
  MaxDist = abs(YREF1 - YREF2) # Distance from the back of the nest to the entrance
  
  Colony18CirclePreDist <<- data.table %>% # Data table to run through the function
    mutate(DistanceX = XREF2 - X, # X axis distance to the entrance
           DistanceY = max(Y) - Y, # Y axis distance to the entrance
           PythagDist = sqrt((DistanceX^2) + (DistanceY^2)), # Shortest distance to the nest entrance 
           ScaledDist = PythagDist / MaxDist, # Scaling the distances by the max possible distance
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>% # If any distance is greater than 1 (precautionary), then it's assigned a 1
    ungroup() %>% # Ungroup the data set
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the circle nest entrance function for the Colony18CirclePre data set
DistanceCoordsFunctionTrackerCircleBase18(Colony18CirclePre)

# COLONY 20
# TUBE NEST
# INVADER
DistanceCoordsFunctionTrackerTubeAggn20 <- function(data.table){
  TubeCoords <- AggnStudyRefCoords %>% # Reference data set for eight even area bins
    filter(Colony == "20" & Trial == "Aggn" & Nest == "Tube") %>% # Filter out only tube nest aggression assay references
    dplyr::select(c(Coord, XREF, YREF)) # Select only desired columns 
  
  # X axis references
  XCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(YREF)) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF1 = as.numeric(XCoords[,1]) # X axis reference from column 1 from above
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  XREF3 = as.numeric(XCoords[,3]) # X axis reference from column 3 from above
  XREF4 = as.numeric(XCoords[,4]) # X axis reference from column 4 from above
  XREF5 = as.numeric(XCoords[,5]) # X axis reference from column 5 from above
  XREF6 = as.numeric(XCoords[,6]) # X axis reference from column 6 from above
  XREF7 = as.numeric(XCoords[,7]) # X axis reference from column 7 from above
  
  # Y axis references
  YCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(XREF)) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # Y axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # Y axis reference from column 2 from above
  YREF3 = as.numeric(YCoords[,3]) # Y axis reference from column 3 from above
  YREF4 = as.numeric(YCoords[,4]) # Y axis reference from column 4 from above
  YREF5 = as.numeric(YCoords[,5]) # Y axis reference from column 5 from above
  YREF6 = as.numeric(YCoords[,6]) # Y axis reference from column 6 from above
  YREF7 = as.numeric(YCoords[,7]) # Y axis reference from column 7 from above
  
  # Width of the tube nest
  Width = YREF2 - YREF3
  
  # Reference distances within the tube nest
  Dist1 = sqrt(((XREF1 - XREF2)^2) + ((YREF1 - YREF2)^2)) # Distance to the entrance from corner entering bin 1 from bin 2
  Dist2 = sqrt(((XREF2 - XREF3)^2) + ((YREF2 - YREF3)^2)) + Dist1 # Distance to the entrance from bin 3 entering bin 2
  Dist3 = sqrt(((XREF4 - XREF3)^2) + ((YREF3 - YREF4)^2)) + Dist2 # Distance to the entrance from bin 4, 5, 6 entering bin 3
  Dist4 = sqrt(((XREF5 - XREF4)^2) + ((YREF5 - YREF4)^2)) + Dist3 # Distance to the entrance from bin 7 entering bin 6
  Dist5 = sqrt(((XREF6 - XREF5)^2) + ((YREF6 - YREF5)^2)) + Dist4 # Distance to the entrance from bin 8 entering bin 7
  MaxDist = sqrt(((XREF6 - XREF7)^2) + ((YREF6 - YREF7)^2)) + Dist5 # Distance from the back of the nest to the entrance
  
  Colony20TubeAggnDist <<- data.table %>% # Data table to run through the function
    filter(Nest == "Tube") %>% # Filter out tube nest coordinates
    # Coordinate bin assignment
    mutate(X1 = ifelse(X >= XREF2 & X <= (XREF2 + (1.25 * Width)) & Y >= YREF3, # If the x axiss of each coordinate is in bin 1
                       "YES", "NO"), 
           X2 = ifelse(X < XREF2 & Y >= YREF3, # If the x axis of each coordinate is in bin 2
                       "YES", "NO"),
           X3 = ifelse(X <= XREF4 & Y < YREF3, # If the x axis of each coordinate is in bin 3
                       "YES", "NO"),
           X4 = ifelse(X > XREF4 & Y <= YREF5, # If the x axis of each coordinate is in bins 4, 5, or 6
                       "YES", "NO"),
           X5 = ifelse(X >= XREF6 & Y > YREF5, # If the x axis of each coordinate is in bin 7
                       "YES", "NO"),
           X6 = ifelse(X >= XREF7 & Y >= YREF7, # If the x axis of each coordinate is in bin 8
                       "YES", "NO"),
           Y1 = ifelse(X1 == "YES", "YES", "NO"), # If the coordinate qualified under X1
           Y2 = ifelse(X2 == "YES", "YES", "NO"), # If the coordinate qualified under X2
           Y3 = ifelse(X3 == "YES", "YES", "NO"), # If the coordinate qualified under X3
           Y4 = ifelse(X4 == "YES", "YES", "NO"), # If the coordinate qualified under X4
           Y5 = ifelse(X5 == "YES", "YES", "NO"), # If the coordinate qualified under X5
           Y6 = ifelse(X6 == "YES", "YES", "NO"), # If the coordinate qualified under X6
           # X axis distances
           DistanceX = ifelse(X1 == "YES", # Bin 1
                              X - XREF1,
                              ifelse(X2 == "YES", # Bin 2
                                     XREF2 - X,
                                     ifelse(X3 == "YES", # Bin 3
                                            XREF3 - X,
                                            ifelse(X4 == "YES", # Bins 4-6
                                                   X - XREF4,
                                                   ifelse(X5 == "YES", # Bin 7
                                                          X - XREF5,
                                                          ifelse(X6 == "YES", # Bin 8
                                                                 XREF6 - X,
                                                                 NA)))))), # Else NA
           # Y axis distances
           DistanceY = ifelse(Y1 == "YES", # Bin 1
                              YREF1 - Y,
                              ifelse(Y2 == "YES", # Bin 2
                                     YREF2 - Y,
                                     ifelse(Y3 == "YES" | Y4 == "YES" & X < XREF4, # Bin 3, or bin 4 with x coordinates less than the bin's x axis reference
                                            YREF3 - Y,
                                            ifelse(Y4 == "YES", # Bin 4-6
                                                   YREF4 - Y,
                                                   ifelse(Y5 == "YES", # Bin 7
                                                          Y - YREF5,
                                                          ifelse(Y6 == "YES", # Bin 8
                                                                 YREF6 - Y,
                                                                 NA)))))),
           # Shortest distances from the nest entrance
           # Only bin 1 is a direct line to the entrance, every other bin is the shortest distance to the closest bin to the entrance + reference distance for that bin to the entrance
           PythagDist = ifelse(X1 == "YES" & Y1 == "YES", # Bin 1
                               sqrt((DistanceX^2) + (DistanceY^2)),
                               ifelse(X2 == "YES" & Y2 == "YES", # Bin 2
                                      sqrt((DistanceX^2) + (DistanceY^2)) + Dist1,
                                      ifelse(X3 == "YES" & Y3 == "YES", # Bin 3
                                             sqrt((DistanceX^2) + (DistanceY^2)) + Dist2,
                                             ifelse(X4 == "YES" & Y4 == "YES", # Bin 4-6
                                                    sqrt((DistanceX^2) + (DistanceY^2)) + Dist3, 
                                                    ifelse(X5 == "YES" & Y5 == "YES", # Bin 7
                                                           sqrt((DistanceX^2) + (DistanceY^2)) + Dist4,
                                                           ifelse(X6 == "YES" & Y6 == "YES", # Bin 8
                                                                  sqrt((DistanceX^2) + (DistanceY^2)) + Dist5,
                                                                  NA)))))),
           # Scaling distances by the max possible distance
           ScaledDist = PythagDist / MaxDist,
           # If any distance is greater than 1 (precautionary), then it's assigned a 1
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>%
    ungroup() %>% # Ungroup the data
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the tube nest entrance function for the Colony20TubeAggn data set
DistanceCoordsFunctionTrackerTubeAggn20(Colony20TubeAggn)

# BASELINE
DistanceCoordsFunctionTrackerTubeBase20 <- function(data.table){
  TubeCoords <- AggnStudyRefCoords %>% # Reference data set for eight even area bins
    filter(Colony == "20" & Trial == "Pre" & Nest == "Tube") %>% # Filter out only tube nest baseline assay references
    dplyr::select(c(Coord, XREF, YREF)) # Select only desired columns 
  
  # X axis references
  XCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(YREF)) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF1 = as.numeric(XCoords[,1]) # X axis reference from column 1 from above
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  XREF3 = as.numeric(XCoords[,3]) # X axis reference from column 3 from above
  XREF4 = as.numeric(XCoords[,4]) # X axis reference from column 4 from above
  XREF5 = as.numeric(XCoords[,5]) # X axis reference from column 5 from above
  XREF6 = as.numeric(XCoords[,6]) # X axis reference from column 6 from above
  XREF7 = as.numeric(XCoords[,7]) # X axis reference from column 7 from above
  
  # Y axis references
  YCoords <- TubeCoords %>% # Creating a x axis reference data set
    dplyr::select(-c(XREF)) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # Y axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # Y axis reference from column 2 from above
  YREF3 = as.numeric(YCoords[,3]) # Y axis reference from column 3 from above
  YREF4 = as.numeric(YCoords[,4]) # Y axis reference from column 4 from above
  YREF5 = as.numeric(YCoords[,5]) # Y axis reference from column 5 from above
  YREF6 = as.numeric(YCoords[,6]) # Y axis reference from column 6 from above
  YREF7 = as.numeric(YCoords[,7]) # Y axis reference from column 7 from above
  
  # Width of the tube nest
  Width = YREF2 - YREF3
  
  # Reference distances within the tube nest
  Dist1 = sqrt(((XREF1 - XREF2)^2) + ((YREF1 - YREF2)^2)) # Distance to the entrance from corner entering bin 1 from bin 2
  Dist2 = sqrt(((XREF2 - XREF3)^2) + ((YREF2 - YREF3)^2)) + Dist1 # Distance to the entrance from bin 3 entering bin 2
  Dist3 = sqrt(((XREF4 - XREF3)^2) + ((YREF3 - YREF4)^2)) + Dist2 # Distance to the entrance from bin 4, 5, 6 entering bin 3
  Dist4 = sqrt(((XREF5 - XREF4)^2) + ((YREF5 - YREF4)^2)) + Dist3 # Distance to the entrance from bin 7 entering bin 6
  Dist5 = sqrt(((XREF6 - XREF5)^2) + ((YREF6 - YREF5)^2)) + Dist4 # Distance to the entrance from bin 8 entering bin 7
  MaxDist = sqrt(((XREF6 - XREF7)^2) + ((YREF6 - YREF7)^2)) + Dist5 # Distance from the back of the nest to the entrance
  
  Colony20TubePreDist <<- data.table %>% # Data table to run through the function
    filter(Nest == "Tube") %>% # Filter out tube nest coordinates
    # Coordinate bin assignment
    mutate(X1 = ifelse(X >= XREF2 & X <= (XREF2 + (1.25 * Width)) & Y >= YREF3, # If the x axiss of each coordinate is in bin 1
                       "YES", "NO"), 
           X2 = ifelse(X < XREF2 & Y >= YREF3, # If the x axis of each coordinate is in bin 2
                       "YES", "NO"),
           X3 = ifelse(X <= XREF4 & Y < YREF3, # If the x axis of each coordinate is in bin 3
                       "YES", "NO"),
           X4 = ifelse(X > XREF4 & Y <= YREF5, # If the x axis of each coordinate is in bins 4, 5, or 6
                       "YES", "NO"),
           X5 = ifelse(X >= XREF6 & Y > YREF5, # If the x axis of each coordinate is in bin 7
                       "YES", "NO"),
           X6 = ifelse(X >= XREF7 & Y >= YREF7, # If the x axis of each coordinate is in bin 8
                       "YES", "NO"),
           Y1 = ifelse(X1 == "YES", "YES", "NO"), # If the coordinate qualified under X1
           Y2 = ifelse(X2 == "YES", "YES", "NO"), # If the coordinate qualified under X2
           Y3 = ifelse(X3 == "YES", "YES", "NO"), # If the coordinate qualified under X3
           Y4 = ifelse(X4 == "YES", "YES", "NO"), # If the coordinate qualified under X4
           Y5 = ifelse(X5 == "YES", "YES", "NO"), # If the coordinate qualified under X5
           Y6 = ifelse(X6 == "YES", "YES", "NO"), # If the coordinate qualified under X6
           # X axis distances
           DistanceX = ifelse(X1 == "YES", # Bin 1
                              X - XREF1,
                              ifelse(X2 == "YES", # Bin 2
                                     XREF2 - X,
                                     ifelse(X3 == "YES", # Bin 3
                                            XREF3 - X,
                                            ifelse(X4 == "YES", # Bins 4-6
                                                   X - XREF4,
                                                   ifelse(X5 == "YES", # Bin 7
                                                          X - XREF5,
                                                          ifelse(X6 == "YES", # Bin 8
                                                                 XREF6 - X,
                                                                 NA)))))), # Else NA
           # Y axis distances
           DistanceY = ifelse(Y1 == "YES", # Bin 1
                              YREF1 - Y,
                              ifelse(Y2 == "YES", # Bin 2
                                     YREF2 - Y,
                                     ifelse(Y3 == "YES" | Y4 == "YES" & X < XREF4, # Bin 3, or bin 4 with x coordinates less than the bin's x axis reference
                                            YREF3 - Y,
                                            ifelse(Y4 == "YES", # Bin 4-6
                                                   YREF4 - Y,
                                                   ifelse(Y5 == "YES", # Bin 7
                                                          Y - YREF5,
                                                          ifelse(Y6 == "YES", # Bin 8
                                                                 YREF6 - Y,
                                                                 NA)))))),
           # Shortest distances from the nest entrance
           # Only bin 1 is a direct line to the entrance, every other bin is the shortest distance to the closest bin to the entrance + reference distance for that bin to the entrance
           PythagDist = ifelse(X1 == "YES" & Y1 == "YES", # Bin 1
                               sqrt((DistanceX^2) + (DistanceY^2)),
                               ifelse(X2 == "YES" & Y2 == "YES", # Bin 2
                                      sqrt((DistanceX^2) + (DistanceY^2)) + Dist1,
                                      ifelse(X3 == "YES" & Y3 == "YES", # Bin 3
                                             sqrt((DistanceX^2) + (DistanceY^2)) + Dist2,
                                             ifelse(X4 == "YES" & Y4 == "YES", # Bin 4-6
                                                    sqrt((DistanceX^2) + (DistanceY^2)) + Dist3, 
                                                    ifelse(X5 == "YES" & Y5 == "YES", # Bin 7
                                                           sqrt((DistanceX^2) + (DistanceY^2)) + Dist4,
                                                           ifelse(X6 == "YES" & Y6 == "YES", # Bin 8
                                                                  sqrt((DistanceX^2) + (DistanceY^2)) + Dist5,
                                                                  NA)))))),
           # Scaling distances by the max possible distance
           ScaledDist = PythagDist / MaxDist,
           # If any distance is greater than 1 (precautionary), then it's assigned a 1
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>%
    ungroup() %>% # Ungroup the data
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the tube nest entrance function for the Colony20TubePre data set
DistanceCoordsFunctionTrackerTubeBase20(Colony20TubePre)

# CIRCLE NEST
# INVADER
DistanceCoordsFunctionTrackerCircleAggn20 <-function(data.table){
  CircleCoords <- AggnStudyCircleRefCoords %>% # Reference coordinates for the circle nest
    filter(Colony == "20" & Trial == "Aggn" & Nest == "Circle") %>% # Filtering out the aggression assay references
    dplyr::select(c("Coord", "XREF", "YREF")) # Select only the desired columns
  
  # X axis references
  XCoords <- CircleCoords %>% # Creating a x axis reference data set
    dplyr::select(-c("YREF")) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  
  # Y axis references
  YCoords <- CircleCoords %>% # Creating a y axis reference data set
    dplyr::select(-c("XREF")) # Remove x axis references
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # X axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # X axis reference from column 2 from above
  MaxDist = abs(YREF1 - YREF2) # Distance from the back of the nest to the entrance
  
  Colony20CircleAggnDist <<- data.table %>% # Data table to run through the function
    mutate(DistanceX = XREF2 - X, # X axis distance to the entrance
           DistanceY = max(Y) - Y, # Y axis distance to the entrance
           PythagDist = sqrt((DistanceX^2) + (DistanceY^2)), # Shortest distance to the nest entrance 
           ScaledDist = PythagDist / MaxDist, # Scaling the distances by the max possible distance
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>% # If any distance is greater than 1 (precautionary), then it's assigned a 1
    ungroup() %>% # Ungroup the data set
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the circle nest entrance function for the Colony11CircleAggn data set
DistanceCoordsFunctionTrackerCircleAggn20(Colony20CircleAggn)

# BASELINE
DistanceCoordsFunctionTrackerCircleBase20 <- function(data.table){
  CircleCoords <- AggnStudyCircleRefCoords %>% # Reference coordinates for the circle nest
    filter(Colony == "20" & Trial == "Pre" & Nest == "Circle") %>% # Filtering out the baseline assay references
    dplyr::select(c("Coord", "XREF", "YREF")) # Select only the desired columns
  
  # X axis references
  XCoords <- CircleCoords %>% # Creating a x axis reference data set
    dplyr::select(-c("YREF")) # Remove y axis references
  XCoords <- t(XCoords) # Transpose the x axis reference coordinates, which means that each reference will be it's own column now
  XCoords <- row_to_names(XCoords, row_number = 1) # Add row names, which are the previous column names
  XREF2 = as.numeric(XCoords[,2]) # X axis reference from column 2 from above
  
  # Y axis references
  YCoords <- CircleCoords %>% # Creating a y axis reference data set
    dplyr::select(-c("XREF")) # Remove x axis references
  YCoords <- t(YCoords) # Transpose the y axis reference coordinates, which means that each reference will be it's own column now
  YCoords <- row_to_names(YCoords, row_number = 1) # Add row names, which are the previous column names
  YREF1 = as.numeric(YCoords[,1]) # X axis reference from column 1 from above
  YREF2 = as.numeric(YCoords[,2]) # X axis reference from column 2 from above
  MaxDist = abs(YREF1 - YREF2) # Distance from the back of the nest to the entrance
  
  Colony20CirclePreDist <<- data.table %>% # Data table to run through the function
    mutate(DistanceX = XREF2 - X, # X axis distance to the entrance
           DistanceY = max(Y) - Y, # Y axis distance to the entrance
           PythagDist = sqrt((DistanceX^2) + (DistanceY^2)), # Shortest distance to the nest entrance 
           ScaledDist = PythagDist / MaxDist, # Scaling the distances by the max possible distance
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist)) %>% # If any distance is greater than 1 (precautionary), then it's assigned a 1
    ungroup() %>% # Ungroup the data set
    mutate(DistBin = cut_width(ScaledDist, width = 0.05, boundary = 0), # Create a sequence from the ScaledDist column at every 0.05 distance, where the value is a bin (e.g. 0.05,0.10)
           DistBin = gsub("\\[|\\]", "", DistBin), # Remove the extra [ that forms around the DistBin column when you use cut_width
           DistBin = gsub("\\(|\\)", "", DistBin)) %>% # Remove the extra [ that forms around the DistBin column when you use cut_width 
    full_join(FiveDistList) %>% # Join the distance bins reference from above
    group_by(ID, DistBin) %>% # Group by the worker ID and distance bin columns
    mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average speed of workers in each 0.05 scaled distance bin
    dplyr::select(c("Colony", "Nest", "Trial", "AvgSpeed", "DistBin", "ScaledDist", "Distance", "ID", "Seconds")) %>% # Select only desired columns
    distinct() # Remove any duplicates
}

# Run the distance to the circle nest entrance function for the Colony20CirclePre data set
DistanceCoordsFunctionTrackerCircleBase20(Colony20CirclePre)

##########################################################################################################
# DATA PROCESSING, ANALYSES, VISUALIZATION: WORKER SPEED OVER TIME
# Producing the final data sets for each set of analyses and visualizations:
# Average worker speed in each assay (two-second bins)
# Average worker speed (twenty-second bins)
# Average interquartile range of average worker speeds in twenty-second intervals
##########################################################################################################

# WORKER SPEEDS OVER TIME 
# INVADER ASSAY
# Full join a subset of the aggression assay data sets (prevents too much computer memory use)
AggnAssayTest<- 
  Colony9Aggn %>%
  full_join(Colony13Aggn) %>%
  full_join(Colony18Aggn) %>%
  full_join(Colony20Aggn) %>%
  full_join(Colony6Aggn) %>%
  full_join(Colony7Aggn) %>%
  full_join(Colony8Aggn) %>%
  dplyr::select(Colony, Nest, Trial, Seconds, AntLength.sec, ID) # Select only the desired columns

# Full join the remaining subset of the aggression assay data sets 
AggnAssayTest1 <- Colony5Aggn %>%
  full_join(Colony11Aggn) %>%
  full_join(Colony17Aggn) %>%
  dplyr::select(Colony, Nest, Trial, Seconds, AntLength.sec, ID)# Select only the desired columns

# Final aggression assay data set
AggnAssayTestFullRaw <- full_join(AggnAssayTest, AggnAssayTest1)

# BASELINE ASSAY
# Full join a subset of the baseline assay data sets (prevents too much computer memory use)
PreAssayTest <-
  Colony9Pre %>%
  full_join(Colony5Pre) %>%
  full_join(Colony13Pre) %>%
  full_join(Colony18Pre) %>%
  full_join(Colony20Pre) %>%
  full_join(Colony6Pre) %>%
  full_join(Colony7Pre) %>%
  dplyr::select(Colony, Nest, Trial, Seconds, AntLength.sec, ID) # Select only the desired columns

# Full join the remaining subset of the baseline assay data sets 
PreAssayTest1 <- Colony8Pre %>%
  full_join(Colony9Pre) %>%
  full_join(Colony11Pre) %>%
  full_join(Colony17Pre) %>%
  dplyr::select(Colony, Nest, Trial, Seconds, AntLength.sec, ID) # Select only the desired columns

# Final baseline assay data set
PreAssayTestFullRaw <- full_join(PreAssayTest, PreAssayTest1)

# FULL DATA SET
FullAssayTestSpeed <- full_join(AggnAssayTestFullRaw, PreAssayTestFullRaw)

# Remove the data sets that were combined, you don't need them and they take up a lot of computer memory 
#rm("Colony5Aggn", "Colony5Pre", "Colony6Aggn", "Colony6Pre",
 #  "Colony7Aggn", "Colony7Pre", "Colony8Aggn", "Colony8Pre",
  # "Colony9Aggn", "Colony9Pre", "Colony11Aggn", "Colony11Pre",
   #"Colony13Aggn", "Colony13Pre", "Colony17Aggn", "Colony17Pre",
   #"Colony18Aggn", "Colony18Pre", "Colony20Aggn", "Colony20Pre",
   #"AggnAssayTest", "AggnAssayTest1", "PreAssayTest", "PreAssayTest1")

# CREATE THE FINAL DATA SETS FOR ANALYSES AND VISUALIZATION 
# Bins the worker speeds into two and twenty second time intervals

# INVADER ASSAY
# Two second 
AggnAssayTestFullTwoSec <- AggnAssayTestFullRaw %>% # Raw aggression assay data set (no binning)
  group_by(Colony, Nest) %>% # Group by the Colony and Nest columns
  mutate(Seconds = cut_width(Seconds, width = 2, boundary = 0), # Create a sequence from the Seconds column at every 2 seconds, where the value is a bin (e.g. 2,4)
         Seconds = gsub("\\[|\\]", "", Seconds),
         Seconds = gsub("\\(|\\)", "", Seconds)) %>%
  left_join(TwoSecsList)

# Twenty second
AggnAssayTestFull <- AggnAssayTestFullRaw %>% # Raw aggression assay data set (no binning)
  group_by(Colony, Nest) %>% # Group by the Colony and Nest columns
  mutate(Seconds = cut_width(Seconds, width = 20, boundary = 0), # Create a sequence from the Seconds column at every 20 seconds, where the value is a bin (e.g. 20,40)
         Seconds = gsub("\\[|\\]", "", Seconds), # Remove the extra [ that forms around the Seconds column when you use cut_width
         Seconds = gsub("\\(|\\)", "", Seconds)) %>% # Remove the extra ( that forms around the Seconds column when you use cut_width)
  left_join(TwentySecBin) # Left join the twenty-second bin data set

# BASELINE ASSAY 
# Two second 
PreAssayTestFullTwoSec <- PreAssayTestFullRaw %>% # Raw baseline assay data set (no binning)
  group_by(Colony, Nest) %>% # Group by the Colony and Nest columns
  mutate(Seconds = cut_width(Seconds, width = 2, boundary = 0), # Create a sequence from the Seconds column at every 2 seconds, where the value is a bin (e.g. 2,4)
         Seconds = gsub("\\[|\\]", "", Seconds), # Remove the extra [ that forms around the Seconds column when you use cut_width
         Seconds = gsub("\\(|\\)", "", Seconds)) %>% # Remove the extra ( that forms around the Seconds column when you use cut_width)
  left_join(TwoSecsList) # Left join the two-second bin data set

# Twenty second
PreAssayTestFull <- PreAssayTestFullRaw %>% # Raw baseline assay data set (no binning)
  group_by(Colony, Nest) %>% # Group by the Colony and Nest columns
  mutate(Seconds = cut_width(Seconds, width = 20, boundary = 0), # Create a sequence from the Seconds column at every 20 seconds, where the value is a bin (e.g. 20,40)
         Seconds = gsub("\\[|\\]", "", Seconds), # Remove the extra [ that forms around the Seconds column when you use cut_width
         Seconds = gsub("\\(|\\)", "", Seconds)) %>% # Remove the extra ( that forms around the Seconds column when you use cut_width)
  left_join(TwentySecBin) # Left join the twenty-second bin data set

# Full two second bin data set
FullAssayTestSpeedTwoSec <- full_join(AggnAssayTestFullTwoSec, PreAssayTestFullTwoSec) %>% # Join the aggression and baseline assays two-second binned data sets
  group_by(Colony, Nest, Trial, Seconds, ID) %>% # Group by the desired columns
  mutate(AvgSpeed = mean (AntLength.sec)) %>% # Average worker speed 
  dplyr::select(-c(AntLength.sec)) %>% # Remove the raw worker speed column
  distinct() %>% # Remove any duplicates
  ungroup() # Ungroup the data

# OVERALL SPEED 
# Full comparison in each assay
AggnAssayTestFullAvg <- AggnAssayTestFull %>%
  filter(AntLength.sec < 10) %>% # Remove any data points that are usually 1 frame artifacts, they are always above 10 ant-lengths / sec
  group_by(Colony, ID, Secs20) %>% # Group by the desired columns
  mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average the worker speeds within the twenty second interval ("Secs20" column)
  ungroup() %>% # Ungroup the data
  dplyr::select(-c(AntLength.sec)) %>% # Remove the unwanted column
  distinct() # Remove any duplicates

PreAssayTestFullAvg <- PreAssayTestFull %>%
  filter(AntLength.sec < 10) %>% # Remove any data points that are usually 1 frame artifacts, they are always above 10 ant-lengths / sec
  group_by(Colony, ID, Secs20) %>% # Group by the desired columns
  mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average the worker speeds within the twenty second interval ("Secs20" column)
  ungroup() %>% # Ungroup the data
  dplyr::select(-c(AntLength.sec)) %>% # Remove the unwanted column
  distinct() # Remove any duplicates

# INDIVIDUAL NEST AND ASSAY COMBINATIONS
# INVADER ASSAY
# CIRCLE NEST
AggnAssayTestFullCircle <- AggnAssayTestFull %>%
  filter(Nest == "Circle" & AntLength.sec < 10) %>% # Keep only circle nest data and remove any data points that are usually 1 frame artifacts, they are always above 10 ant-lengths / sec
  group_by(Colony, ID, Secs20) %>% # Group by the desired columns
  mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average the worker speeds within the twenty second interval ("Secs20" column)
  ungroup() %>% # Ungroup the data
  dplyr::select(-c(AntLength.sec)) %>% # Remove the unwanted column
  distinct() # Remove any duplicates

# TUBE NEST
AggnAssayTestFullTube <- AggnAssayTestFull %>%
  filter(Nest == "Tube" & AntLength.sec < 10) %>% # Keep only circle nest data and remove any data points that are usually 1 frame artifacts, they are always above 10 ant-lengths / sec
  group_by(Colony, ID, Secs20) %>% # Group by the desired columns
  mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average the worker speeds within the twenty second interval ("Secs20" column)
  ungroup() %>% # Ungroup the data
  dplyr::select(-c(AntLength.sec)) %>% # Remove the unwanted column
  distinct() # Remove any duplicates

# BASELINE ASSAY
# CIRCLE NEST
PreAssayTestFullCircle <- PreAssayTestFull %>%
  filter(Nest == "Circle" & AntLength.sec < 10) %>% # Remove any data points that are usually 1 frame artifacts, they are always above 10 ant-lengths / sec
  group_by(Colony, ID, Secs20) %>% # Group by the desired columns
  mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average the worker speeds within the twenty second interval ("Secs20" column)
  ungroup() %>% # Ungroup the data
  dplyr::select(-c(AntLength.sec)) %>% # Remove the unwanted column
  distinct() # Remove any duplicates

# TUBE NEST
PreAssayTestFullTube <- PreAssayTestFull %>%
  filter(Nest == "Tube" & AntLength.sec < 10) %>% # Remove any data points that are usually 1 frame artifacts, they are always above 10 ant-lengths / sec
  group_by(Colony, ID, Secs20) %>% # Group by the desired columns
  mutate(AvgSpeed = mean(AntLength.sec)) %>% # Average the worker speeds within the twenty second interval ("Secs20" column)
  ungroup() %>% # Ungroup the data
  dplyr::select(-c(AntLength.sec)) %>% # Remove the unwanted column
  distinct() # Remove any duplicates

# MAX VALUES: SPEED OVER TIME
# Calculating the max worker speed values in each nest and assay combination

# Function that rounds data to the nearest thousandths 
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall = 3))

# OVERALL INVADER ASSAY DATA
AggnMax = AggnAssayTestFull %>% # Aggression assay full data set
  filter(AntLength.sec < 10) %>% # Remove any data points that are usually 1 frame artifacts, they are always above 10 ant-lengths / sec
  group_by(Nest) %>% # Group by the nest column
  mutate(MaxValues = specify_decimal(max(AntLength.sec), 3)) %>% # Create a column of max values, rounded to the nearest thousandths 
  dplyr::select(Nest, MaxValues) %>% # Select the desires columns
  ungroup() %>% # Ungroup the data
  distinct() # Remove duplicates

# OVERALL BASELINE ASSAY DATA
PreMax = PreAssayTestFull %>% # Baseline assay full data set
  filter(AntLength.sec < 10) %>% # Remove any data points that are usually 1 frame artifacts, they are always above 10 ant-lengths / sec
  group_by(Nest) %>% # Group by the nest column
  mutate(MaxValues = specify_decimal(max(AntLength.sec), 3)) %>% # Create a column of max values, rounded to the nearest thousandths 
  dplyr::select(Nest, MaxValues) %>% # Select the desires columns
  ungroup() %>% # Ungroup the data
  distinct() # Remove duplicates

# CIRCLE NEST
# BASELINE ASSAY
CirclePreMax = PreAssayTestFull %>% # Baseline assay circle nest data set
  filter(Nest == "Circle" & AntLength.sec < 10) %>% # Remove any data points that are usually 1 frame artifacts, they are always above 10 ant-lengths / sec
  group_by(Secs20) %>% # Group by the twenty-second bin column
  mutate(MaxValues = specify_decimal(max(AntLength.sec), 3)) %>% # Create a column of max values, rounded to the nearest thousandths 
  dplyr::select(Trial, Nest, Secs20, MaxValues) %>% # Select the desires columns
  ungroup() %>% # Ungroup the data
  distinct() # Remove duplicates

# INVADER ASSAY
CircleAggnMax = AggnAssayTestFull %>% # Baseline assay tube nest data set
  filter(Nest == "Circle" & AntLength.sec < 10) %>% # Remove any data points that are usually 1 frame artifacts, they are always above 10 ant-lengths / sec
  group_by(Secs20) %>% # Group by the twenty-second bin column
  mutate(MaxValues = specify_decimal(max(AntLength.sec), 3)) %>% # Create a column of max values, rounded to the nearest thousandths 
  dplyr::select(Trial, Nest, Secs20, MaxValues) %>% # Select the desires columns
  ungroup() %>% # Ungroup the data
  distinct() # Remove duplicates

# TUBE NEST
# BASELINE ASSAY
TubePreMax = PreAssayTestFull %>% # Aggression assay tube nest data set
  filter(Nest == "Tube" & AntLength.sec < 10) %>% # Remove any data points that are usually 1 frame artifacts, they are always above 10 ant-lengths / sec
  group_by(Secs20) %>% # Group by the twenty-second bin column
  mutate(MaxValues = specify_decimal(max(AntLength.sec), 3)) %>% # Create a column of max values, rounded to the nearest thousandths 
  dplyr::select(Trial, Nest, Secs20, MaxValues) %>% # Select the desires columns
  ungroup() %>% # Ungroup the data
  distinct() # Remove duplicates

# TUBE NEST
# INVADER ASSAY
TubeAggnMax = AggnAssayTestFull %>% # Aggression assay circle nest data set
  filter(Nest == "Tube" & AntLength.sec < 10) %>% # Remove any data points that are usually 1 frame artifacts, they are always above 10 ant-lengths / sec
  group_by(Secs20) %>% # Group by the twenty-second bin column
  mutate(MaxValues = specify_decimal(max(AntLength.sec), 3)) %>% # Create a column of max values, rounded to the nearest thousandths 
  dplyr::select(Trial, Nest, Secs20, MaxValues) %>% # Select the desires columns
  ungroup() %>% # Ungroup the data
  distinct() # Remove duplicates

##########################################################################################################
# FIGURES: OVERALL WORKER SPEED OVER TIME & SAMPLE SIZE
# (1) Boxplots of the average worker speed (two-second intervals) in the aggression v baseline assays in each nest
# (2) Boxplots of the average worker speed (across twenty-second intervals) in the aggression v baseline assays in each nest
# (3) Sample sizes for the aggression and baseline assays: mean and standard deviation
##########################################################################################################

# OVERALL AVERAGE WORKER SPEED (INVADER V BASELINE)
# BASELINE ASSAY
PreTime <-
  ggplot(data = FullAssayTestSpeedTwoSec, aes(y = AvgSpeed, x = Nest, fill = Nest)) +
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00), alpha = 0.65) +
  stat_summary(geom = 'text', label = PreMax$MaxValues, 
               fun = max, 
               hjust = 0.5,
               vjust = -0.5,
               size = 6) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Baseline") +
  theme_pubclean() +
  theme(axis.text.x = element_text(size = 18, color = "black"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 18, color = "black"),
        legend.key = element_blank(),
        legend.position = "none",
        legend.justification = c(1, 1),
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        plot.title = element_text(size = 20, color = "black")) +
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name = "Nest",
                    values = c("blue", "red")) +
  ylim(0, 0.01)

# INVADER ASSAY 
AggnTime <-
  ggplot(data = FullAssayTestSpeedTwoSec, aes(y = AvgSpeed, x = Nest, fill = Nest)) +
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00), alpha = 0.65) +
  stat_summary(geom = 'text', label = AggnMax$MaxValues, 
               fun = max, 
               hjust = 0.5,
               vjust = -0.5,
               size = 6) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Invader") +
  theme_pubclean() +
  theme(axis.text.x = element_text(size = 18, color = "black"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 18, color = "white"),
        legend.key = element_blank(),
        legend.position = "none",
        legend.justification = c(1, 1),
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        plot.title = element_text(size = 20, color = "black")) +
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name = "Nest",
                    values = c("blue", "red")) +
  ylim(0, 0.01)

# Compiling the plots 
SpeedFig <- ggarrange(PreTime, AggnTime,
                     labels = c("(a)", "(b)"),
                     ncol = 2, nrow = 1,
                     font.label = list(size = 20, color = "black", face = "plain"))

# Annotate the compiled plots and produce common x and y axes
Speed_Fig <- annotate_figure(SpeedFig,
                top = NULL,
                bottom = text_grob("Nest shape", color = "black",
                                   size = 20, x = 0.53),
                left = text_grob("Average ant-lengths / sec", color = "black",
                                 size = 20, rot = 90),
                right = NULL
)

# Save plot as a PDF
ggsave(file = "Fig5.pdf", plot = Speed_Fig, width = 10.4, height = 6.75, units = "in")


# SAMPLE SIZES: AVERAGE WORKER SPEED (TWO-SECOND BINS) FOR EACH ASSAY
# INVADER ASSAY
AggnAssayTestFullTwoSec %>% # Full aggression assay (two-second binned) data set
  group_by(Nest, Trial, Seconds) %>% # Group by the desired columns
  mutate(Count = n()) %>% # Count the number of rows in each group
  ungroup() %>% # Ungroup the data
  mutate(AvgCount = mean(Count), # Average number of rows in each group
         StdDev = sd(Count)) %>% # Standard deviation number of rows in each group
  dplyr::select(c(AvgCount), StdDev) %>% # Select the mean and standard deviation columns only
  distinct() # Remove duplicates

# BASELINE ASSAY
PreAssayTestFullTwoSec %>% # Full baseline assay (two-second binned) data set
  group_by(Nest, Trial, Seconds) %>% # Group by the desired columns
  mutate(Count = n()) %>% # Count the number of rows in each group
  ungroup() %>% # Ungroup the data
  mutate(AvgCount = mean(Count), # Average number of rows in each group
         StdDev = sd(Count)) %>% # Standard deviation number of rows in each group
  dplyr::select(c(AvgCount), StdDev) %>% # Select the mean and standard deviation columns only
  distinct() # Remove duplicates

# LINEAR MIXED EFFECTS MODEL
# RESPONSE VARIABLE 
# AvgSpeed - The average speed of workers in two-second bins over the 5-min video
# FIXED EFFECTS
# Nest - Nest shape (Tube / Circle)
# Trial - The assay of the video (Aggn / Pre), note that pre is the baseline assay
# RANDOM EFFECT
# (1|Colony) - Colony identification 
summary(lmer(AvgSpeed ~ Nest * Trial + (1|Colony), data = FullAssayTestSpeedTwoSec))

# Marginal and conditional R-squared, showing the influence of the random effect on the model
r.squaredGLMM(lmer(AvgSpeed ~ Nest * Trial + (1|Colony), data = FullAssayTestSpeedTwoSec))

# AVERAGE WORKER SPEED, INDIVIDUAL NESTS IN EACH ASSAY
# CIRCLE NEST
# BASELINE ASSAY
Circle.PreTime <- ggplot(data = PreAssayTestFullCircle, aes(y = AvgSpeed, x = as.factor(Secs20))) +
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00), alpha = 0.65, fill = "blue") +
  ggtitle("Circle Baseline") +
  xlab(NULL) +
  ylab(NULL) +
  theme_pubclean() +
  ylim(0, 0.04) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 16, color = "black"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 18, color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 20, color = "black"))

# INVADER ASSAY
Circle.AggnTime <-
  ggplot(data = AggnAssayTestFullCircle, aes(y = AvgSpeed, x = factor(Secs20))) +
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00), alpha = 0.65, fill = "blue") +
  ggtitle("Circle Invader") +
  xlab(NULL) +
  ylab(NULL) +
  theme_pubclean() +
  ylim(0, 0.04) +
  theme(axis.text.x = element_text(angle = 50,hjust = 1, size = 18, color = "black"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 18, color = "white"),
        axis.title = element_blank(),
        plot.title = element_text(size = 20, face = "plain", color = "black"))

# TUBE NEST
# BASELINE ASSAY
Tube.PreTime <- ggplot(data = PreAssayTestFullTube, aes(y = AvgSpeed, x = as.factor(Secs20))) +
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00), alpha = 0.65, fill = "red") +
  ggtitle("Tube Baseline") +
  xlab(NULL) +
  ylab(NULL) +
  theme_pubclean() +
  ylim(0, 0.04) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 18, color = "black"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 18, color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 20, color = "black"))

# INVADER ASSAY
Tube.AggnTime <- ggplot(data = AggnAssayTestFullTube, aes(y = AvgSpeed, x = as.factor(Secs20))) +
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00), alpha = 0.65, fill = "red") +
  ggtitle("Tube Invader") +
  xlab(NULL) +
  ylab(NULL) +
  theme_pubclean() +
  ylim(0, 0.04) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 18, color = "black"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 18, color = "white"),
        axis.title = element_blank(),
        plot.title = element_text(size = 20, color = "black"))

# Compiling the plots 
SpeedFigFull <- ggarrange(Circle.PreTime, Circle.AggnTime,
                         Tube.PreTime, Tube.AggnTime,
                         labels = c("(a)", "(b)", "(c)", "(d)"),
                         font.label = list(size = 20, face = "plain", color = "black"),
                         ncol = 2, nrow = 2)

# Annotate the compiled plots and produce common x and y axes
Speed_FigFull <- annotate_figure(SpeedFigFull,
                top = NULL,
                bottom = text_grob("Time (s)", color = "black",
                                   size = 20, x = 0.53),
                left = text_grob("Average ant-lengths / sec", color = "black",
                                 size = 20, rot = 90),
                right = NULL
)

# Save plot as a PDF
ggsave(file = "FigS8.pdf", plot = Speed_FigFull, width = 10.4, height = 10.4, units = "in")


##########################################################################################################
# AVERAGE WORKER SPEED (TWENTY-SECOND INTERVALS) INTERQUARTILE RANGE (IQR)
# Calculating and visualizing the average worker speed IQR 
##########################################################################################################

# IQR CALCULATIONS FOR OVERALL AVERAGE WORKER SPEED IN EACH ASSAY 
# INVADER ASSAY
AggnAssayTestIQR <- AggnAssayTestFullTwoSec %>% # Raw worker movement speed data
  group_by(Colony, Nest) %>% # Group by the Colony and Nest columns 
  group_by(Colony, Nest, Secs) %>% # Group by the desired columns
  mutate(IQRSecs = IQR(AntLength.sec)) %>% # Calculate the IQR of average worker speeds in twenty-second intervals
  dplyr::select(-c(AntLength.sec)) %>% # Remove the raw worker speed column
  distinct() %>% # Remove duplicates
  group_by(Colony, Nest) %>% # Make sure the same grouping exists
  mutate(AvgIQR = mean(IQRSecs)) %>% # Average the IQR values for each colony and nest combination
  dplyr::select(Colony, Nest, Trial, AvgIQR) %>% # Select the desired columns
  distinct() # Remove duplicates

# BASELINE ASSAY
PreAssayTestIQR <- PreAssayTestFullTwoSec %>% # Raw worker movement speed data
  group_by(Colony, Nest) %>% # Group by the Colony and Nest columns 
  group_by(Colony, Nest, Secs) %>% # Group by the desired columns
  mutate(IQRSecs = IQR(AntLength.sec)) %>% # Calculate the IQR of average worker speeds in twenty-second intervals
  dplyr::select(-c(AntLength.sec)) %>% # Remove the raw worker speed column
  distinct() %>% # Remove duplicates
  group_by(Colony, Nest) %>% # Make sure the same grouping exists
  mutate(AvgIQR = mean(IQRSecs)) %>% # Average the IQR values for each colony and nest combination
  dplyr::select(Colony, Nest, Trial, AvgIQR) %>% # Select the desired columns
  distinct() # Remove duplicates

# Full join the IQR data sets
FullAssayTestIQR <- full_join(AggnAssayTestIQR, PreAssayTestIQR)

##########################################################################################################
# FIGURES: OVERALL WORKER SPEED OVER TIME IQR
# Boxplots of the IQR of average worker speed (twenty-second intervals) in the aggression v baseline assays in each nest
##########################################################################################################

# OVERALL AVERAGE WORKER SPEED IQR (INVADER V BASELINE)
# BASELINE ASSAY
SpeedPreIQR <- ggplot(data = FullAssayTestIQR %>% filter(Trial == "Pre"), aes(y = AvgIQR, x = Nest, fill = Nest)) +
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00), alpha = 0.65) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Baseline") +
  theme_pubclean() +
  theme(axis.text.x = element_text(size = 18, color = "black"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 18, color = "black"),
        legend.key = element_blank(),
        legend.position = "none",
        legend.justification = c(1, 1),
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        plot.title = element_text(size = 20, color = "black")) +
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name = "Nest",
                    values = c("blue", "red")) + 
  ylim(0, 0.004)

# INVADER ASSAY 
SpeedAggnIQR <- ggplot(data = FullAssayTestIQR %>% filter(Trial == "Aggn"), aes(y = AvgIQR, x = Nest, fill = Nest)) +
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00), alpha = 0.65) +
  annotate("segment", x = 0.0001, xend = 0.45, y = 0.004, yend = 0.004, linetype = "solid", color = "gray50", size = 1) +
  annotate("text", x = 0.001, y = 0.002, label = "Plot (a) y-lim", color = "gray40", hjust = 0, size = 6) +
  annotate("text", x = 0.001, y = 0.005, label = "0.004", color = "gray40", hjust = 0, size = 6) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Invader") +
  theme_pubclean() +
  theme(axis.text.x = element_text(size = 18, color = "black"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 18, color = "black"),
        legend.key = element_blank(),
        legend.position = "none",
        legend.justification = c(1, 1),
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        plot.title = element_text(size = 20, color = "black")) +
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name = "Nest",
                    values = c("blue", "red")) +
  ylim(0, 0.03)

# Compiling the plots
SpeedIQRFig <- ggarrange(SpeedPreIQR, SpeedAggnIQR,
                        labels = c("(a)", "(b)"),
                        ncol = 2, nrow = 1,
                        font.label = list(size = 20,
                                          color = "black", face = "plain"))

# Annotating the compiled plots and produce common x and y axes
SpeedIQR_Fig <- annotate_figure(SpeedIQRFig,
                top = NULL,
                bottom = text_grob("Nest shape", color = "black",
                                   size = 20, x = 0.53),
                left = text_grob("Average IQR ant-lengths / sec", color = "black",
                                 size = 20, rot = 90),
                right = NULL
)

# Save plot as a PDF
ggsave(file = "Fig6.pdf", plot = SpeedIQR_Fig, width = 10.4, height = 6.75, units = "in")


# LINEAR MIXED EFFECTS MODEL
# RESPONSE VARIABLE 
# AvgIQR - The average IQR values calculated from twenty-second binned average speed of workers over the 5-min video
# FIXED EFFECTS
# Nest - Nest shape (Tube / Circle)
# Trial - The assay of the video (Aggn / Pre), note that pre is the baseline assay
# RANDOM EFFECT
# (1|Colony) - Colony identification 
summary(lmer(AvgIQR ~ Nest * Trial + (1|Colony), data = FullAssayTestIQR))

# Marginal and conditional R-squared, showing the influence of the random effect on the model
r.squaredGLMM(lmer(AvgIQR ~ Nest * Trial + (1|Colony), data = FullAssayTestIQR))

##########################################################################################################
# DATA PROCESSING, ANALYSES, VISUALIZATION: WORKER SPEED OVER SPACE 
# Producing the final data sets for each set of analyses and visualizations:
# Average worker speed (two-second bins) in scaled distance from the entrance bins
# Average interquartile range of average worker speed (two-second bins) in scaled distance from the entrance bins
##########################################################################################################

# COMBINE THE INDIVIDUAL COLONY SCALED DISTANCE FROM THE NEST ENTRANCE DATA SETS
# INVADER ASSAY
Colony5AggnDist <- full_join(Colony5CircleAggnDist, Colony5TubeAggnDist)
Colony6AggnDist <- full_join(Colony6CircleAggnDist, Colony6TubeAggnDist)
Colony7AggnDist <- full_join(Colony7CircleAggnDist, Colony7TubeAggnDist)
Colony8AggnDist <- full_join(Colony8CircleAggnDist, Colony8TubeAggnDist)
Colony9AggnDist <- full_join(Colony9CircleAggnDist, Colony9TubeAggnDist)
Colony11AggnDist <- full_join(Colony11CircleAggnDist, Colony11TubeAggnDist)
Colony13AggnDist <- full_join(Colony13CircleAggnDist, Colony13TubeAggnDist)
Colony17AggnDist <- full_join(Colony17CircleAggnDist, Colony17TubeAggnDist)
Colony18AggnDist <- full_join(Colony18CircleAggnDist, Colony18TubeAggnDist)
Colony20AggnDist <- full_join(Colony20CircleAggnDist, Colony20TubeAggnDist)

# Full join a subset of the aggression assay data sets (prevents too much computer memory use)
AggnAssayDistTest <-
  Colony5AggnDist %>%
  full_join(Colony6AggnDist) %>%
  full_join(Colony7AggnDist) %>%
  full_join(Colony8AggnDist) %>%
  full_join(Colony9AggnDist)

# Full join the remaining subset of the aggression assay data sets 
AggnAssayDistTest1 <- Colony13AggnDist%>%
  full_join(Colony17AggnDist) %>%
  full_join(Colony18AggnDist) %>%
  full_join(Colony20AggnDist) %>%
  full_join(Colony11AggnDist)

# Final aggression assay data set
AggnAssayDistTestFull <- full_join(AggnAssayDistTest, AggnAssayDistTest1) %>%
  drop_na() %>% # Drop NAs
  dplyr::select(-c(ScaledDist)) %>% # Remove the ScaledDist column so that average distances remain
  distinct() # Remove duplicates

# BASELINE ASSAY
Colony5PreDist <- full_join(Colony5TubePreDist, Colony5CirclePreDist)
Colony6PreDist <- full_join(Colony6CirclePreDist, Colony6TubePreDist)
Colony7PreDist <- full_join(Colony7TubePreDist, Colony7CirclePreDist)
Colony8PreDist <- full_join(Colony8CirclePreDist, Colony8TubePreDist)
Colony9PreDist <- full_join(Colony9CirclePreDist, Colony9TubePreDist)
Colony11PreDist <- full_join(Colony11CirclePreDist, Colony11TubePreDist)
Colony13PreDist <- full_join(Colony13CirclePreDist, Colony13TubePreDist)
Colony17PreDist <- full_join(Colony17CirclePreDist, Colony17TubePreDist)
Colony18PreDist <- full_join(Colony18CirclePreDist, Colony18TubePreDist)
Colony20PreDist <- full_join(Colony20CirclePreDist, Colony20TubePreDist)

# Full join a subset of the baseline assay data sets (prevents too much computer memory use)
PreAssayDistTest <- Colony13PreDist %>%
  full_join(Colony18PreDist) %>%
  full_join(Colony20PreDist) %>%
  full_join(Colony6PreDist) %>%
  full_join(Colony7PreDist) %>%
  full_join(Colony5PreDist)

# Full join the remaining subset of the baseline assay data sets 
PreAssayDistTest1 <- Colony8PreDist %>%
  full_join(Colony9PreDist) %>%
  full_join(Colony17PreDist) %>%
  full_join(Colony11PreDist)

# Final baseline assay data set
PreAssayDistTestFull <- full_join(PreAssayDistTest, PreAssayDistTest1) %>%
  drop_na() %>% # Drop NAs
  dplyr::select(-c(ScaledDist)) %>% # Remove the ScaledDist column so that average distances remain
  distinct() # Remove duplicates

# MAX VALUES: SPEED OVER SPACE
# Calculating the max worker speed values across distances from the nest entrance in each nest and assay combination

# CIRCLE NEST
# INVADER ASSAY
CircleDistAggnMax = AggnAssayDistTestFull %>% # Full aggression assay data set for average worker speeds across scaled distance to the nest entrance bins
  drop_na() %>% # Drop NAs
  filter(Nest == "Circle") %>% # Filter only circle nest coordinates
  group_by(Distance) %>% # Group by the distance bin
  mutate(MaxValues = signif(max(AvgSpeed), 2)) %>% # Calculate the maximum value of average worker speed in each distance bin
  dplyr::select(Distance, MaxValues) %>% # Select only the distance bin and maximum values
  ungroup() %>% # Ungroup the data 
  distinct() # Remove duplicates

# BASELINE ASSAY
CircleDistPreMax = PreAssayDistTestFull %>% # Full baseline assay data set for average worker speeds across scaled distance to the nest entrance bins
  drop_na() %>% # Drop NAs
  filter(Nest == "Circle") %>% # Filter only circle nest coordinates
  group_by(Distance) %>% # Group by the distance bin
  mutate(MaxValues = signif(max(AvgSpeed), 2)) %>% # Calculate the maximum value of average worker speed in each distance bin
  dplyr::select(Distance, MaxValues) %>% # Select only the distance bin and maximum values
  ungroup() %>% # Ungroup the data 
  distinct() # Remove duplicates

# TUBE NEST
# INVADER ASSAY
TubeDistAggnMax = AggnAssayDistTestFull %>% # Full aggression assay data set for average worker speeds across scaled distance to the nest entrance bins
  drop_na() %>% # Drop NAs
  filter(Nest == "Tube") %>% # Filter only circle nest coordinates
  group_by(Distance) %>% # Group by the distance bin
  mutate(MaxValues = signif(max(AvgSpeed), 2)) %>% # Calculate the maximum value of average worker speed in each distance bin
  dplyr::select(Distance, MaxValues) %>% # Select only the distance bin and maximum values
  ungroup() %>% # Ungroup the data 
  distinct() # Remove duplicates

# BASELINE ASSAY 
TubeDistPreMax = PreAssayDistTestFull %>% # Full baseline assay data set for average worker speeds across scaled distance to the nest entrance bins
  drop_na() %>% # Drop NAs
  filter(Nest == "Tube") %>% # Filter only circle nest coordinates
  group_by(Distance) %>% # Group by the distance bin
  mutate(MaxValues = signif(max(AvgSpeed), 2)) %>% # Calculate the maximum value of average worker speed in each distance bin
  dplyr::select(Distance, MaxValues) %>% # Select only the distance bin and maximum values
  ungroup() %>% # Ungroup the data 
  distinct() # Remove duplicates

# AVERAGE WORKER SPEED ACROSS SCALED DISTANCE TO THE NEST ENTRANCE BINS, INDIVIDUAL NESTS IN EACH ASSAY
# CIRCLE NEST
# BASELINE ASSAY
Circ.PreDist <- ggplot(data = PreAssayDistTestFull %>% filter(Nest == "Circle"), aes(y = AvgSpeed, x = as.factor(Distance))) +
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00), fill = "blue", alpha = 0.65) +
  ggtitle("Circle Baseline") +
  xlab(NULL) +
  ylab(NULL) +
  theme_pubclean() +
  ylim(0, 0.045) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 18, color = "black"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 18, color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 20, color = "black"))

# INVADER ASSAY
Circ.AggnDist <- ggplot(data = AggnAssayDistTestFull %>% filter(Nest == "Circle"), aes(y = AvgSpeed, x = as.factor(Distance))) +
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00),
                      fill = "blue", alpha = 0.65) +  
  ggtitle("Circle Invader") +
  xlab(NULL) +
  ylab(NULL) +
  theme_pubclean() +
  ylim(0, 0.045) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 18, color = "black"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 18, color = "white"),
        plot.title = element_text(size = 20, color = "black"))
  
# TUBE NEST
# BASELINE ASSAY 
Tube.PreDist <- ggplot(data = PreAssayDistTestFull %>% filter(Nest == "Tube"), aes(y = AvgSpeed, x = as.factor(Distance))) +
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00),
                      fill = "red", alpha = 0.65) +  
  ggtitle("Tube Baseline") +
  xlab(NULL) +
  ylab(NULL) +
  theme_pubclean() +
  ylim(0, 0.045) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 18, color = "black"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 18, color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 20, color = "black"))

# INVADER ASSAY
Tube.AggnDist <- ggplot(data = AggnAssayDistTestFull %>% filter(Nest == "Tube"), aes(y = AvgSpeed, x = as.factor(Distance))) +
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00),
                      fill = "red", alpha = 0.65) +  
  ggtitle("Tube Invader") +
  xlab(NULL) +
  ylab(NULL) +
  theme_pubclean() +
  ylim(0, 0.045) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 18, color = "black"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 18, color = "white"),
        plot.title = element_text(size = 20, color = "black"))

# Compiling the plots
SpeedDist <- ggarrange(Circ.PreDist, Circ.AggnDist,
          Tube.PreDist, Tube.AggnDist,
          labels = c("(a)", "(b)", "(c)", "(d)"),
          font.label = list(size = 20,
                              face = "plain", color = "black"),
          ncol = 2, nrow = 2)

# Annotating the compiled plots to include common x and y axes
SpeedDist_Full <- annotate_figure(SpeedDist,
                top = NULL,
                bottom = text_grob("Scaled distance to the nest entrance", color = "black",
                                   size = 20, x = 0.53),
                left = text_grob("Average ant-lengths / sec", color = "black",
                                 size = 20, rot = 90),
                right = NULL
)

# Save plot as a PDF
ggsave(file = "FigS9.pdf", plot = SpeedDist_Full, width = 10.4, height = 10.4, units = "in")


##########################################################################################################
# AVERAGE WORKER SPEED (SCALED DISTANCE TO THE NEST ENTRANCE) INTERQUARTILE RANGE (IQR)
# Calculating and visualizing the average worker speed in each 0.05 scaled distance to the nest entrance interval  IQR 
##########################################################################################################

# IQR CALCULATIONS FOR OVERALL AVERAGE WORKER SPEED ACROSS NEST SPACE IN EACH ASSAY 
# INVADER ASSAY
# Calculate for a subset of data (else the computer may crash)
AggnAssayTestDistIQR <- AggnAssayDistTest %>% # Aggression assay distance data set # 1
  drop_na() %>% # Drop NAs
  ungroup() %>% # Ungroup the data
  dplyr::select(Colony, Nest, Distance, AvgSpeed, Trial) %>% # Select the desired columns
  group_by(Colony, Nest, Distance) %>% # Group by the desired columns
  mutate(IQRDist = IQR(as.numeric(AvgSpeed))) %>% # Calculate the IQR of average worker speeds in each 0.05 scaled distance to the nest entrance bin
  dplyr::select(-c(AvgSpeed)) %>% # Remove the average speed column
  distinct() %>% # Remove duplicates
  group_by(Colony, Nest) %>% # Group by the Colony and Nest columns
  mutate(AvgIQR = mean(IQRDist)) %>% # Calculate the average IQR value
  dplyr::select(Colony, Nest, Trial, AvgIQR) %>% # Select the desired columns 
  distinct() # Remove duplicates

# Calculate for the remaining data 
AggnAssayTestDistIQR1 <- AggnAssayDistTest1 %>% # Aggression assay distance data set # 2
  drop_na() %>% # Drop NAs
  ungroup() %>% # Ungroup the data
  dplyr::select(Colony, Nest, Distance, AvgSpeed, Trial) %>% # Select the desired columns
  group_by(Colony, Nest, Distance) %>% # Group by the desired columns
  mutate(IQRDist = IQR(as.numeric(AvgSpeed))) %>% # Calculate the IQR of average worker speeds in each 0.05 scaled distance to the nest entrance bin
  dplyr::select(-c(AvgSpeed)) %>% # Remove the average speed column
  distinct() %>% # Remove duplicates
  group_by(Colony, Nest) %>% # Group by the Colony and Nest columns
  mutate(AvgIQR = mean(IQRDist)) %>% # Calculate the average IQR value
  dplyr::select(Colony, Nest, Trial, AvgIQR) %>% # Select the desired columns 
  distinct() # Remove duplicates

# BASELINE ASSAY
PreAssayTestDistIQR <- PreAssayDistTest %>% # Baseline assay distance data set # 1
  drop_na() %>% # Drop NAs
  ungroup() %>% # Ungroup the data
  dplyr::select(Colony, Nest, Distance, AvgSpeed, Trial) %>% # Select the desired columns
  group_by(Colony, Nest, Distance) %>% # Group by the desired columns
  mutate(IQRDist = IQR(as.numeric(AvgSpeed))) %>% # Calculate the IQR of average worker speeds in each 0.05 scaled distance to the nest entrance bin
  dplyr::select(-c(AvgSpeed)) %>% # Remove the average speed column
  distinct() %>% # Remove duplicates
  group_by(Colony, Nest) %>% # Group by the Colony and Nest columns
  mutate(AvgIQR = mean(IQRDist)) %>% # Calculate the average IQR value
  dplyr::select(Colony, Nest, Trial, AvgIQR) %>% # Select the desired columns 
  distinct() # Remove duplicates

PreAssayTestDistIQR1 <- PreAssayDistTest1 %>% # Baseline assay distance data set # 1
  drop_na() %>% # Drop NAs
  ungroup() %>% # Ungroup the data
  dplyr::select(Colony, Nest, Distance, AvgSpeed, Trial) %>% # Select the desired columns
  group_by(Colony, Nest, Distance) %>% # Group by the desired columns
  mutate(IQRDist = IQR(as.numeric(AvgSpeed))) %>% # Calculate the IQR of average worker speeds in each 0.05 scaled distance to the nest entrance bin
  dplyr::select(-c(AvgSpeed)) %>% # Remove the average speed column
  distinct() %>% # Remove duplicates
  group_by(Colony, Nest) %>% # Group by the Colony and Nest columns
  mutate(AvgIQR = mean(IQRDist)) %>% # Calculate the average IQR value
  dplyr::select(Colony, Nest, Trial, AvgIQR) %>% # Select the desired columns 
  distinct() # Remove duplicates

# Full join all average IQR distance data sets
FullAssayTestDistIQR <- AggnAssayTestDistIQR %>%
  full_join(PreAssayTestDistIQR) %>%
  full_join(AggnAssayTestDistIQR1) %>%
  full_join(PreAssayTestDistIQR1)

##########################################################################################################
# FIGURES: OVERALL WORKER SPEED OVER NEST SPACE IQRs
# Boxplots of the IQR of average worker speed (0.05 scaled distance from the nest entrance bins) in the aggression v baseline assays in each nest
##########################################################################################################

# OVERALL AVERAGE WORKER SPEED IQR DISTANCE (INVADER V BASELINE)
# BASELINE ASSAY 
SpeedPreDistIQR <- ggplot(data = FullAssayTestDistIQR %>% filter(Trial == "Pre"), aes(y = AvgIQR, x = Nest, fill = Nest)) +
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00), alpha = 0.65) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Baseline") +
  theme_pubclean() +
  theme(axis.text.x = element_text(size = 18, color = "black"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 18, color = "black"),
        legend.key = element_blank(),
        legend.position = "none",
        legend.justification = c(1, 1),
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        plot.title = element_text(size = 20, color = "black")) +
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name = "Nest",
                    values = c("blue", "red")) +
  ylim(0, 0.004)

# INVADER ASSAY
SpeedAggnDistIQR <- ggplot(data = FullAssayTestDistIQR %>% filter(Trial == "Aggn"), aes(y = AvgIQR, x = Nest, fill = Nest)) +
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00), alpha = 0.65) +
  annotate("segment", x = 0.0001, xend = 0.45, y = 0.004, yend = 0.004, linetype = "solid", color = "gray50", size = 1) +
  annotate("text", x = 0.001, y = 0.002, label = "Plot (a) y-lim", color = "gray40", hjust = 0, size = 6) +
  annotate("text", x = 0.001, y = 0.003, label = "0.004", color = "gray40", hjust = 0, size = 6) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Invader") +
  theme_pubclean() + 
  theme(axis.text.x = element_text(size = 18, color = "black"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 18, color = "black"),
        legend.key = element_blank(),
        legend.position = "none",
        legend.justification = c(1, 1),
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        plot.title = element_text(size = 20, color = "black")) +
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name = "Nest",
                    values = c("blue", "red")) +
  ylim(0, 0.015)

# Compiling the plots
SpeedDistIQR <- ggarrange(SpeedPreDistIQR, SpeedAggnDistIQR,
                         labels = c("(a)", "(b)"),
                         ncol = 2, nrow = 1,
                         font.label = list(size = 20,
                                             face = "plain"))

# Annotating the compiled plots and produce common x and y axes
SpeedDist_IQR<- annotate_figure(SpeedDistIQR,
                top = NULL,
                bottom = text_grob("Nest shape", color = "black",
                                   size = 20, x = 0.53),
                left = text_grob("Average IQR avg. speed / distance bin", color = "black",
                                 size = 20, rot = 90),
                right = NULL
)

# Save plot as a PDF
ggsave(file = "Fig7.pdf", plot = SpeedDist_IQR, width = 10.4, height = 6.75, units = "in")


# LINEAR MIXED EFFECTS MODEL
# RESPONSE VARIABLE 
# AvgIQR - The average IQR values calculated from 0.05 scaled distance to the nest entrance binned average speed of workers over the 5-min video
# FIXED EFFECTS
# Nest - Nest shape (Tube / Circle)
# Trial - The assay of the video (Aggn / Pre), note that pre is the baseline assay
# RANDOM EFFECT
# (1|Colony) - Colony identification 
summary(lmer(AvgIQR ~ Nest * Trial + (1|Colony), data = FullAssayTestDistIQR))

# Marginal and conditional R-squared, showing the influence of the random effect on the model
r.squaredGLMM(lmer(AvgIQR ~ Nest * Trial + (1|Colony), data = FullAssayTestDistIQR))
