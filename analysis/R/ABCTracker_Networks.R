##########################################################################################################
## Author: GREG CHISM 
## Date: FEB 2023 
## email: gchism@.arizona.edu
## Project: Nest shape does not affect ant colony performance against a nest invader despite altered worker movement and communication
## Title: Worker interaction networks 
##########################################################################################################

####### DESCRIPTION OF THE SCRIPT##########
# Network production from adjacency matrices produced from ABCTracker_AdjacencyMatrix.R
# Network measures related to colony performance
# Network comparative measures
# All associated visualization and analyses

# READ ME: 
# You will need to run all of the code from ABCTracker_Movement.R needed to produce the distance from the nest entrance datasets for each video

##########################################################################################################
# INSTALL & LOAD REQUIRED PACKAGES
##########################################################################################################
# Download package with function to load multiple packaged at once
if (!require(pacman)) install.packages('pacman')

pacman::p_load(assertthat,
               CINNA,
               ggpubr,
               ggraph,
               graphlayouts,
               hablar,
               here,
               igraph,
               lme4,
               lmerTest,
               MASS,
               matrixStats,
               MuMIn,
               reshape2, # Loading required packages for code below. p_load() will download packages that aren't in system library
               scales, 
               sjPlot, 
               tidyverse,
               wesanderson)


##########################################################################################################
# LOAD THE REQUIRED DATA SETS
# (1) Import and combine all matrices, while also removing imported database after they are combined (they are redundant and take up memory)
# (2) Create matrices from the combined datasets
##########################################################################################################

# COLONY 5
# AGGRESSION ASSAY
# TUBE NEST
# NON-RECIPROCAL
Colony5TubeAggnNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony5TubeAggnNRMatrix.csv"), row.names = 1, header = TRUE)

# RECIPROCAL
Colony5TubeAggnRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony5TubeAggnRMatrix.csv"), row.names = 1, header = TRUE)

# Matrix addition of the two matrices
Colony5TubeAggnMatrix <- as.matrix(Colony5TubeAggnNRMatrix + Colony5TubeAggnRMatrix)

# Remove the original adjacency matrices
rm("Colony5TubeAggnNRMatrix", "Colony5TubeAggnRMatrix")

# CIRCLE NEST
# NON-RECIPROCAL
Colony5CircleAggnNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony5CircleAggnNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony5CircleAggnRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony5CircleAggnRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony5CircleAggnMatrix <- as.matrix(Colony5CircleAggnNRMatrix + Colony5CircleAggnRMatrix)

# Remove the original adjacency matrices
rm("Colony5CircleAggnNRMatrix", "Colony5CircleAggnRMatrix")

# BASELINE ASSAY
# TUBE NEST
# NON-RECIPROCAL
Colony5TubePreNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony5TubePreNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony5TubePreRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony5TubePreRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony5TubePreMatrix <- as.matrix(Colony5TubePreNRMatrix + Colony5TubePreRMatrix)

# Remove the original adjacency matrices
rm("Colony5TubePreNRMatrix", "Colony5TubePreRMatrix")

# CIRCLE NEST
# NON-RECIPROCAL
Colony5CirclePreNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony5CirclePreNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony5CirclePreRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony5CirclePreRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony5CirclePreMatrix <- as.matrix(Colony5CirclePreNRMatrix + Colony5CirclePreRMatrix)

# Remove the original adjacency matrices
rm("Colony5CirclePreNRMatrix", "Colony5CirclePreRMatrix")

# COLONY 6
# AGGRESSION ASSAY
# TUBE NEST
# NON-RECIPROCAL
Colony6TubeAggnNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony6TubeAggnNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony6TubeAggnRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony6TubeAggnRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony6TubeAggnMatrix <- as.matrix(Colony6TubeAggnNRMatrix + Colony6TubeAggnRMatrix)

# Remove the original adjacency matrices
rm("Colony6TubeAggnNRMatrix", "Colony6TubeAggnRMatrix")

# CIRCLE NEST
# NON-RECIPROCAL
Colony6CircleAggnNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony6CircleAggnNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony6CircleAggnRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony6CircleAggnRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony6CircleAggnMatrix <- as.matrix(Colony6CircleAggnNRMatrix + Colony6CircleAggnRMatrix)

# Remove the original adjacency matrices
rm("Colony6CircleAggnNRMatrix", "Colony6CircleAggnRMatrix")

# BASELINE ASSAY
# TUBE NEST
# NON-RECIPROCAL
Colony6TubePreNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony6TubePreNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony6TubePreRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony6TubePreRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony6TubePreMatrix <- as.matrix(Colony6TubePreNRMatrix + Colony6TubePreRMatrix)

# Remove the original adjacency matrices
rm("Colony6TubePreNRMatrix", "Colony6TubePreRMatrix")

# CIRCLE NEST
# NON-RECIPROCAL
Colony6CirclePreNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony6CirclePreNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony6CirclePreRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony6CirclePreRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony6CirclePreMatrix <- as.matrix(Colony6CirclePreNRMatrix + Colony6CirclePreRMatrix)

# Remove the original adjacency matrices
rm("Colony6CirclePreNRMatrix", "Colony6CirclePreRMatrix")

# COLONY 7
# AGGRESSION ASSAY
# TUBE NEST
# NON-RECIPROCAL
Colony7TubeAggnNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony7TubeAggnNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony7TubeAggnRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony7TubeAggnRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony7TubeAggnMatrix <- as.matrix(Colony7TubeAggnNRMatrix + Colony7TubeAggnRMatrix)

# Remove the original adjacency matrices
rm("Colony7TubeAggnNRMatrix", "Colony7TubeAggnRMatrix")

# CIRCLE NEST
# NON-RECIPROCAL
Colony7CircleAggnNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony7CircleAggnNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony7CircleAggnRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony7CircleAggnRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony7CircleAggnMatrix <- as.matrix(Colony7CircleAggnNRMatrix + Colony7CircleAggnRMatrix)

# Remove the original adjacency matrices
rm("Colony7CircleAggnNRMatrix", "Colony7CircleAggnRMatrix")

# BASELINE ASSAY
# TUBE NEST
Colony7TubePreNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony7TubePreNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony7TubePreRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony7TubePreRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony7TubePreMatrix <- as.matrix(Colony7TubePreNRMatrix + Colony7TubePreRMatrix)

# Remove the original adjacency matrices
rm("Colony7TubePreNRMatrix", "Colony7TubePreRMatrix")

# CIRCLE NEST
# NON-RECIPROCAL
Colony7CirclePreNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony7CirclePreNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony7CirclePreRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony7CirclePreRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony7CirclePreMatrix <- as.matrix(Colony7CirclePreNRMatrix + Colony7CirclePreRMatrix)

# Remove the original adjacency matrices
rm("Colony7CirclePreNRMatrix", "Colony7CirclePreRMatrix")

# COLONY 8
# AGGRESSION ASSAY
# TUBE NEST
# NON-RECIPROCAL
Colony8TubeAggnNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony8TubeAggnNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony8TubeAggnRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony8TubeAggnRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony8TubeAggnMatrix <- as.matrix(Colony8TubeAggnNRMatrix + Colony8TubeAggnRMatrix)

# Remove the original adjacency matrices
rm("Colony8TubeAggnNRMatrix", "Colony8TubeAggnRMatrix")

# CIRCLE NEST
# NON-RECIPROCAL
Colony8CircleAggnNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony8CircleAggnNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony8CircleAggnRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony8CircleAggnRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony8CircleAggnMatrix <- as.matrix(Colony8CircleAggnNRMatrix + Colony8CircleAggnRMatrix)

# Remove the original adjacency matrices
rm("Colony8CircleAggnNRMatrix", "Colony8CircleAggnRMatrix")

# BASELINE ASSAY
# TUBE NEST
# NON-RECIPROCAL
Colony8TubePreNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony8TubePreNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony8TubePreRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony8TubePreRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony8TubePreMatrix <- as.matrix(Colony8TubePreNRMatrix + Colony8TubePreRMatrix)

# Remove the original adjacency matrices
rm("Colony8TubePreNRMatrix", "Colony8TubePreRMatrix")

# CIRCLE NEST
# NON-RECIPROCAL
Colony8CirclePreNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony8CirclePreNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony8CirclePreRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony8CirclePreRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony8CirclePreMatrix <- as.matrix(Colony8CirclePreNRMatrix + Colony8CirclePreRMatrix)

# Remove the original adjacency matrices
rm("Colony8CirclePreNRMatrix", "Colony8CirclePreRMatrix")

# COLONY 9
# AGGRESSION ASSAY
# TUBE NEST
# NON-RECIPROCAL
Colony9TubeAggnNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony9TubeAggnNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony9TubeAggnRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony9TubeAggnRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony9TubeAggnMatrix <- as.matrix(Colony9TubeAggnNRMatrix + Colony9TubeAggnRMatrix)

# Remove the original adjacency matrices
rm("Colony9TubeAggnNRMatrix", "Colony9TubeAggnRMatrix")

# CIRCLE NEST
# NON-RECIPROCAL
Colony9CircleAggnNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony9CircleAggnNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony9CircleAggnRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony9CircleAggnRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony9CircleAggnMatrix <- as.matrix(Colony9CircleAggnNRMatrix + Colony9CircleAggnRMatrix)

# Remove the original adjacency matrices
rm("Colony9CircleAggnNRMatrix", "Colony9CircleAggnRMatrix")

# BASELINE ASSAY
# TUBE NEST
# NON-RECIPROCAL
Colony9TubePreNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony9TubePreNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony9TubePreRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony9TubePreRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony9TubePreMatrix <- as.matrix(Colony9TubePreNRMatrix + Colony9TubePreRMatrix)

# Remove the original adjacency matrices
rm("Colony9TubePreNRMatrix", "Colony9TubePreRMatrix")

# CIRCLE NEST
# NON-RECIPROCAL
Colony9CirclePreNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony9CirclePreNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony9CirclePreRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony9CirclePreRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony9CirclePreMatrix <- as.matrix(Colony9CirclePreNRMatrix + Colony9CirclePreRMatrix)

# Remove the original adjacency matrices
rm("Colony9CirclePreNRMatrix", "Colony9CirclePreRMatrix")

# COLONY 11
# AGGRESSION ASSAY
# TUBE NEST
# NON-RECIPROCAL
Colony11TubeAggnNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony11TubeAggnNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony11TubeAggnRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony11TubeAggnRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony11TubeAggnMatrix <- as.matrix(Colony11TubeAggnNRMatrix + Colony11TubeAggnRMatrix)

# Remove the original adjacency matrices
rm("Colony11TubeAggnNRMatrix", "Colony11TubeAggnRMatrix")

# CIRCLE NEST
# NON-RECIPROCAL
Colony11CircleAggnNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony11CircleAggnNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony11CircleAggnRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony11CircleAggnRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony11CircleAggnMatrix <- as.matrix(Colony11CircleAggnNRMatrix + Colony11CircleAggnRMatrix)

# Remove the original adjacency matrices
rm("Colony11CircleAggnNRMatrix", "Colony11CircleAggnRMatrix")

# BASELINE ASSAY
# TUBE NEST
# NON-RECIPROCAL
Colony11TubePreNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony11TubePreNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony11TubePreRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony11TubePreRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony11TubePreMatrix <- as.matrix(Colony11TubePreNRMatrix + Colony11TubePreRMatrix)

# Remove the original adjacency matrices
rm("Colony11TubePreNRMatrix", "Colony11TubePreRMatrix")

# CIRCLE NEST
# NON-RECIPROCAL
Colony11CirclePreNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony11CirclePreNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony11CirclePreRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony11CirclePreRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony11CirclePreMatrix <- as.matrix(Colony11CirclePreNRMatrix + Colony11CirclePreRMatrix)

# Remove the original adjacency matrices
rm("Colony11CirclePreNRMatrix", "Colony11CirclePreRMatrix")

# COLONY 13
# AGGRESSION ASSAY
# TUBE NEST
# NON-RECIPROCAL
Colony13TubeAggnNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony13TubeAggnNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony13TubeAggnRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony13TubeAggnRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony13TubeAggnMatrix <- as.matrix(Colony13TubeAggnNRMatrix + Colony13TubeAggnRMatrix)

# Remove the original adjacency matrices
rm("Colony13TubeAggnNRMatrix", "Colony13TubeAggnRMatrix")

# CIRCLE NEST
# NON-RECIPROCAL
Colony13CircleAggnNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony13CircleAggnNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony13CircleAggnRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony13CircleAggnRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony13CircleAggnMatrix <- as.matrix(Colony13CircleAggnNRMatrix + Colony13CircleAggnRMatrix) 

# Remove the original adjacency matrices
rm("Colony13CircleAggnNRMatrix", "Colony13CircleAggnRMatrix")

# BASELINE ASSAY
# TUBE NEST
# NON-RECIPROCAL
Colony13TubePreNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony13TubePreNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony13TubePreRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony13TubePreRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony13TubePreMatrix <- as.matrix(Colony13TubePreNRMatrix + Colony13TubePreRMatrix)

# Remove the original adjacency matrices
rm("Colony13TubePreNRMatrix", "Colony13TubePreRMatrix")

# CIRCLE NEST
# NON-RECIPROCAL
Colony13CirclePreNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony13CirclePreNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony13CirclePreRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony13CirclePreRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony13CirclePreMatrix <- as.matrix(Colony13CirclePreNRMatrix + Colony13CirclePreRMatrix)

# Remove the original adjacency matrices
rm("Colony13CirclePreNRMatrix", "Colony13CirclePreRMatrix")

# COLONY 17
# AGGRESSION ASSAY
# TUBE NEST
# NON-RECIPROCAL
Colony17TubeAggnNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony17TubeAggnNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony17TubeAggnRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony17TubeAggnRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony17TubeAggnMatrix <- as.matrix(Colony17TubeAggnNRMatrix + Colony17TubeAggnRMatrix)

# Remove the original adjacency matrices
rm("Colony17TubeAggnNRMatrix", "Colony17TubeAggnRMatrix")

# CIRCLE NEST
# NON-RECIPROCAL
Colony17CircleAggnNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony17CircleAggnNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony17CircleAggnRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony17CircleAggnRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony17CircleAggnMatrix <- as.matrix(Colony17CircleAggnNRMatrix + Colony17CircleAggnRMatrix)

# Remove the original adjacency matrices
rm("Colony17CircleAggnNRMatrix", "Colony17CircleAggnRMatrix")

# BASELINE ASSAY
# TUBE NEST
# NON-RECIPROCAL
Colony17TubePreNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony17TubePreNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony17TubePreRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony17TubePreRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony17TubePreMatrix <- as.matrix(Colony17TubePreNRMatrix + Colony17TubePreRMatrix)

# Remove the original adjacency matrices
rm("Colony17TubePreNRMatrix", "Colony17TubePreRMatrix")

# CIRCLE NEST
# NON-RECIPROCAL
Colony17CirclePreNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony17CirclePreNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony17CirclePreRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony17CirclePreRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony17CirclePreMatrix <- as.matrix(Colony17CirclePreNRMatrix + Colony17CirclePreRMatrix)

# Remove the original adjacency matrices
rm("Colony17CirclePreNRMatrix", "Colony17CirclePreRMatrix")

# COLONY 18
# AGGRESSION ASSAY
# TUBE NEST
# NON-RECIPROCAL
Colony18TubeAggnNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony18TubeAggnNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony18TubeAggnRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony18TubeAggnRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony18TubeAggnMatrix <- as.matrix(Colony18TubeAggnNRMatrix + Colony18TubeAggnRMatrix)

# Remove the original adjacency matrices
rm("Colony18TubeAggnNRMatrix", "Colony18TubeAggnRMatrix")

# CIRCLE NEST
# NON-RECIPROCAL
Colony18CircleAggnNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony18CircleAggnNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony18CircleAggnRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony18CircleAggnRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony18CircleAggnMatrix <- as.matrix(Colony18CircleAggnNRMatrix + Colony18CircleAggnRMatrix)

# Remove the original adjacency matrices
rm("Colony18CircleAggnNRMatrix", "Colony18CircleAggnRMatrix")

# BASELINE ASSAY
# TUBE NEST
# NON-RECIPROCAL
Colony18TubePreNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony18TubePreNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony18TubePreRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony18TubePreRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony18TubePreMatrix <- as.matrix(Colony18TubePreNRMatrix + Colony18TubePreRMatrix)

# Remove the original adjacency matrices
rm("Colony18TubePreNRMatrix", "Colony18TubePreRMatrix")

# CIRCLE NEST
# NON-RECIPROCAL
Colony18CirclePreNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony18CirclePreNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony18CirclePreRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony18CirclePreRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony18CirclePreMatrix <- as.matrix(Colony18CirclePreNRMatrix + Colony18CirclePreRMatrix)

# Remove the original adjacency matrices
rm("Colony18CirclePreNRMatrix", "Colony18CirclePreRMatrix")

# COLONY 20
# AGGRESSION ASSAY
# TUBE NEST
# NON-RECIPROCAL
Colony20TubeAggnNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony20TubeAggnNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony20TubeAggnRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony20TubeAggnRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony20TubeAggnMatrix <- as.matrix(Colony20TubeAggnNRMatrix + Colony20TubeAggnRMatrix)

# Remove the original adjacency matrices
rm("Colony20TubeAggnNRMatrix", "Colony20TubeAggnRMatrix")

# CIRCLE NEST
# NON-RECIPROCAL
Colony20CircleAggnNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony20CircleAggnNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony20CircleAggnRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony20CircleAggnRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony20CircleAggnMatrix <- as.matrix(Colony20CircleAggnNRMatrix + Colony20CircleAggnRMatrix)

# Remove the original adjacency matrices
rm("Colony20CircleAggnNRMatrix", "Colony20CircleAggnRMatrix")

# BASELINE ASSAY
# TUBE NEST
# NON-RECIPROCAL
Colony20TubePreNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony20TubePreNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony20TubePreRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony20TubePreRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony20TubePreMatrix <- as.matrix(Colony20TubePreNRMatrix + Colony20TubePreRMatrix)

# Remove the original adjacency matrices
rm("Colony20TubePreNRMatrix", "Colony20TubePreRMatrix")

# CIRCLE NEST
# NON-RECIPROCAL
Colony20CirclePreNRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony20CirclePreNRMatrix.csv"), row.names = 1, header = TRUE) 

# RECIPROCAL
Colony20CirclePreRMatrix <- read.csv(here("analysis", "data", "raw_data", "Colony20CirclePreRMatrix.csv"), row.names = 1, header = TRUE) 

# Matrix addition of the two matrices
Colony20CirclePreMatrix <- as.matrix(Colony20CirclePreNRMatrix + Colony20CirclePreRMatrix)

# Remove the original adjacency matrices
rm("Colony20CirclePreNRMatrix", "Colony20CirclePreRMatrix")

##########################################################################################################
# GENERATE NETWORKS
# Generate the weighted (not used) and unweighted matrices from the above adjacency matrices
##########################################################################################################

# COLONY 5
# AGGRESSION ASSAY
# WEIGHTED
# TUBE NEST
Colony5TubeAggnNetwork <- graph_from_adjacency_matrix(Colony5TubeAggnMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony5CircleAggnNetwork <- graph_from_adjacency_matrix(Colony5CircleAggnMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# UNWEIGHTED
# TUBE NEST
# Changing the matrix cells to 1s and 0s (interaction / no interaction)
Colony5TubeAggnUnweighted <- ifelse(Colony5TubeAggnMatrix > 1, 1, Colony5TubeAggnMatrix) # Unweighted matrix, replacing numbers greater than 1 with 1

# CIRCLE NEST
Colony5CircleAggnUnweighted <- ifelse(Colony5CircleAggnMatrix > 1, 1, Colony5CircleAggnMatrix) # Unweighted matrix, replacing numbers greater than 1 with 1

# NETWORK FROM ADJACENCY MATRICES
# TUBE NEST
Colony5TubeAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony5TubeAggnUnweighted) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony5CircleAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony5CircleAggnUnweighted) # Make a network from adjacency matrix above

# BASELINE ASSAY
# WEIGHTED
# TUBE NEST
Colony5TubePreNetwork <- graph_from_adjacency_matrix(Colony5TubePreMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony5CirclePreNetwork <- graph_from_adjacency_matrix(Colony5CirclePreMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# UNWEIGHTED
# TUBE NEST
# Changing the matrix cells to 1s and 0s (interaction / no interaction)
Colony5TubePreUnweighted <- ifelse(Colony5TubePreMatrix > 1, 1, Colony5TubePreMatrix) # Unweighted matrix, replacing numbers greater than 1 with 1

# CIRCLE NEST
Colony5CirclePreUnweighted <- ifelse(Colony5CirclePreMatrix > 1, 1, Colony5CirclePreMatrix) # Unweighted matrix, replacing numbers greater than 1 with 1

# NETWORK FROM ADJACENCY MATRICES
# TUBE NEST
Colony5TubePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony5TubePreUnweighted) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony5CirclePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony5CirclePreUnweighted) # Make a network from adjacency matrix above

# COLONY 6
# AGGRESSION ASSAY
# WEIGHTED
# TUBE NEST
Colony6TubeAggnNetwork <- graph_from_adjacency_matrix(Colony6TubeAggnMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony6CircleAggnNetwork <- graph_from_adjacency_matrix(Colony6CircleAggnMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# UNWEIGHTED
# TUBE NEST
# Changing the matrix cells to 1s and 0s (interaction / no interaction)
Colony6TubeAggnUnweighted <- ifelse(Colony6TubeAggnMatrix > 1, 1, Colony6TubeAggnMatrix) # Unweighted matrix, replacing numbers greater than 1 with 1

# CIRCLE NEST
Colony6CircleAggnUnweighted <- ifelse(Colony6CircleAggnMatrix > 1, 1, Colony6CircleAggnMatrix) # Unweighted matrix, replacing numbers greater than 1 with 1

# NETWORK FROM ADJACENCY MATRICES
# TUBE NEST
Colony6TubeAggnNetworkUnweighted<-graph_from_adjacency_matrix(Colony6TubeAggnUnweighted) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony6CircleAggnNetworkUnweighted<-graph_from_adjacency_matrix(Colony6CircleAggnUnweighted) # Make a network from adjacency matrix above

# BASELINE ASSAY
# WEIGHTED
# TUBE NEST
Colony6TubePreNetwork <- graph_from_adjacency_matrix(Colony6TubePreMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony6CirclePreNetwork <- graph_from_adjacency_matrix(Colony6CirclePreMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# UNWEIGHTED
# TUBE NEST
# Changing the matrix cells to 1s and 0s (interaction / no interaction)
Colony6TubePreUnweighted <- ifelse(Colony6TubePreMatrix > 1, 1, Colony6TubePreMatrix) # Unweighted matrix, replacing numbers greater than 1 with 1

# CIRCLE NEST
Colony6CirclePreUnweighted <- ifelse(Colony6CirclePreMatrix > 1, 1, Colony6CirclePreMatrix) # Unweighted matrix, replacing numbers greater than 1 with 1

# NETWORK FROM ADJACENCY MATRICES
# TUBE NEST
Colony6TubePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony6TubePreUnweighted) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony6CirclePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony6CirclePreUnweighted) # Make a network from adjacency matrix above

# COLONY 7
# AGGRESSION ASSAY
# WEIGHTED
# TUBE NEST
Colony7TubeAggnNetwork <- graph_from_adjacency_matrix(Colony7TubeAggnMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony7CircleAggnNetwork <- graph_from_adjacency_matrix(Colony7CircleAggnMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# UNWEIGHTED
# TUBE NEST
# Changing the matrix cells to 1s and 0s (interaction / no interaction)
Colony7TubeAggnUnweighted <- ifelse(Colony7TubeAggnMatrix > 1, 1, Colony7TubeAggnMatrix) # Unweighted matrix, replacing numbers greater than 1 with 1

# CIRCLE NEST
Colony7CircleAggnUnweighted <- ifelse(Colony7CircleAggnMatrix > 1, 1, Colony7CircleAggnMatrix) # Unweighted matrix, replacing numbers greater than 1 with 1

# NETWORK FROM ADJACENCY MATRICES
# TUBE NEST
Colony7TubeAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony7TubeAggnUnweighted) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony7CircleAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony7CircleAggnUnweighted) # Make a network from adjacency matrix above

# BASELINE ASSAY
# WEIGHTED
# TUBE NEST
Colony7TubePreNetwork <- graph_from_adjacency_matrix(Colony7TubePreMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony7CirclePreNetwork <- graph_from_adjacency_matrix(Colony7CirclePreMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# UNWEIGHTED
# TUBE NEST
# Changing the matrix cells to 1s and 0s (interaction / no interaction)
Colony7TubePreUnweighted <- ifelse(Colony7TubePreMatrix > 1, 1, Colony7TubePreMatrix) # Unweighted matrix, replacing numbers greater than 1 with 1

# CIRCLE NEST
Colony7CirclePreUnweighted <- ifelse(Colony7CirclePreMatrix > 1, 1, Colony7CirclePreMatrix) # Unweighted matrix, replacing numbers greater than 1 with 1

# NETWORK FROM ADJACENCY MATRICES
# TUBE NEST
Colony7TubePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony7TubePreUnweighted) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony7CirclePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony7CirclePreUnweighted) # Make a network from adjacency matrix above

# COLONY 8
# AGGRESSION ASSAY
# WEIGHTED
# TUBE NEST
Colony8TubeAggnNetwork <- graph_from_adjacency_matrix(Colony8TubeAggnMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony8CircleAggnNetwork <- graph_from_adjacency_matrix(Colony8CircleAggnMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# UNWEIGHTED
# TUBE NEST
# Changing the matrix cells to 1s and 0s (interaction / no interaction)
Colony8TubeAggnUnweighted <- ifelse(Colony8TubeAggnMatrix > 1, 1, Colony8TubeAggnMatrix) # Unweighted matrix, replacing numbers greater than 1 with 1

# CIRCLE NEST
Colony8CircleAggnUnweighted <- ifelse(Colony8CircleAggnMatrix > 1, 1, Colony8CircleAggnMatrix) # Unweighted matrix, replacing numbers greater than 1 with 1

# NETWORK FROM ADJACENCY MATRICES
# TUBE NEST
Colony8TubeAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony8TubeAggnUnweighted) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony8CircleAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony8CircleAggnUnweighted) # Make a network from adjacency matrix above

# BASELINE ASSAY
# WEIGHTED
# TUBE NEST
Colony8TubePreNetwork <- graph_from_adjacency_matrix(Colony8TubePreMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony8CirclePreNetwork <- graph_from_adjacency_matrix(Colony8CirclePreMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# UNWEIGHTED
# TUBE NEST
# Changing the matrix cells to 1s and 0s (interaction / no interaction)
Colony8TubePreUnweighted <- ifelse(Colony8TubePreMatrix > 1, 1, Colony8TubePreMatrix)

# CIRCLE NEST
Colony8CirclePreUnweighted <- ifelse(Colony8CirclePreMatrix > 1, 1, Colony8CirclePreMatrix)

# NETWORK FROM ADJACENCY MATRICES
# TUBE NEST
Colony8TubePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony8TubePreUnweighted) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony8CirclePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony8CirclePreUnweighted) # Make a network from adjacency matrix above

# COLONY 9
# AGGRESSION ASSAY
# WEIGHTED
# TUBE NEST
Colony9TubeAggnNetwork <- graph_from_adjacency_matrix(Colony9TubeAggnMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony9CircleAggnNetwork <- graph_from_adjacency_matrix(Colony9CircleAggnMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# UNWEIGHTED
# TUBE NEST
# Changing the matrix cells to 1s and 0s (interaction / no interaction)
Colony9TubeAggnUnweighted <- ifelse(Colony9TubeAggnMatrix > 1, 1, Colony9TubeAggnMatrix)

# CIRCLE NEST
Colony9CircleAggnUnweighted <- ifelse(Colony9CircleAggnMatrix > 1, 1, Colony9CircleAggnMatrix)

# NETWORK FROM ADJACENCY MATRICES
# TUBE NEST
Colony9TubeAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony9TubeAggnUnweighted) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony9CircleAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony9CircleAggnUnweighted) # Make a network from adjacency matrix above

# BASELINE ASSAY
# WEIGHTED
# TUBE NEST
Colony9TubePreNetwork <- graph_from_adjacency_matrix(Colony9TubePreMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony9CirclePreNetwork <- graph_from_adjacency_matrix(Colony9CirclePreMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# UNWEIGHTED
# TUBE NEST
# Changing the matrix cells to 1s and 0s (interaction / no interaction)
Colony9TubePreUnweighted <- ifelse(Colony9TubePreMatrix > 1, 1, Colony9TubePreMatrix)

# CIRCLE NEST
Colony9CirclePreUnweighted <- ifelse(Colony9CirclePreMatrix > 1, 1, Colony9CirclePreMatrix)

# NETWORK FROM ADJACENCY MATRICES
# TUBE NEST
Colony9TubePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony9TubePreUnweighted) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony9CirclePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony9CirclePreUnweighted) # Make a network from adjacency matrix above

# COLONY 11
# AGGRESSION ASSAY
# WEIGHTED
# TUBE NEST
Colony11TubeAggnNetwork <- graph_from_adjacency_matrix(Colony11TubeAggnMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony11CircleAggnNetwork <- graph_from_adjacency_matrix(Colony11CircleAggnMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# UNWEIGHTED
# TUBE NEST
# Changing the matrix cells to 1s and 0s (interaction / no interaction)
Colony11TubeAggnUnweighted <- ifelse(Colony11TubeAggnMatrix > 1, 1, Colony11TubeAggnMatrix)

# CIRCLE NEST
Colony11CircleAggnUnweighted <- ifelse(Colony11CircleAggnMatrix > 1, 1, Colony11CircleAggnMatrix)

# NETWORK FROM ADJACENCY MATRICES
# TUBE NEST
Colony11TubeAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony11TubeAggnUnweighted) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony11CircleAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony11CircleAggnUnweighted) # Make a network from adjacency matrix above

# BASELINE ASSAY
# WEIGHTED
# TUBE NEST
Colony11TubePreNetwork <- graph_from_adjacency_matrix(Colony11TubePreMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony11CirclePreNetwork <- graph_from_adjacency_matrix(Colony11CirclePreMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# UNWEIGHTED
# TUBE NEST
# Changing the matrix cells to 1s and 0s (interaction / no interaction)
Colony11TubePreUnweighted <- ifelse(Colony11TubePreMatrix > 1, 1, Colony11TubePreMatrix)

# CIRCLE NEST
Colony11CirclePreUnweighted <- ifelse(Colony11CirclePreMatrix > 1, 1, Colony11CirclePreMatrix)

# NETWORK FROM ADJACENCY MATRICES
# TUBE NEST
Colony11TubePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony11TubePreUnweighted) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony11CirclePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony11CirclePreUnweighted) # Make a network from adjacency matrix above

# COLONY 13
# AGGRESSION ASSAY
# WEIGHTED
# TUBE NEST
Colony13TubeAggnNetwork <- graph_from_adjacency_matrix(Colony13TubeAggnMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony13CircleAggnNetwork <- graph_from_adjacency_matrix(Colony13CircleAggnMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# UNWEIGHTED
# TUBE NEST
# Changing the matrix cells to 1s and 0s (interaction / no interaction)
Colony13TubeAggnUnweighted <- ifelse(Colony13TubeAggnMatrix > 1, 1, Colony13TubeAggnMatrix)

# CIRCLE NEST
Colony13CircleAggnUnweighted <- ifelse(Colony13CircleAggnMatrix > 1, 1, Colony13CircleAggnMatrix)

# NETWORK FROM ADJACENCY MATRICES
# TUBE NEST
Colony13TubeAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony13TubeAggnUnweighted) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony13CircleAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony13CircleAggnUnweighted) # Make a network from adjacency matrix above

# BASELINE ASSAY
# WEIGHTED
# TUBE NEST
Colony13TubePreNetwork <- graph_from_adjacency_matrix(Colony13TubePreMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony13CirclePreNetwork <- graph_from_adjacency_matrix(Colony13CirclePreMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# UNWEIGHTED
# TUBE NEST
# Changing the matrix cells to 1s and 0s (interaction / no interaction)
Colony13TubePreUnweighted <- ifelse(Colony13TubePreMatrix > 1, 1, Colony13TubePreMatrix)

# CIRCLE NEST
Colony13CirclePreUnweighted <- ifelse(Colony13CirclePreMatrix > 1, 1, Colony13CirclePreMatrix)

# NETWORK FROM ADJACENCY MATRICES
# TUBE NEST
Colony13TubePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony13TubePreUnweighted) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony13CirclePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony13CirclePreUnweighted) # Make a network from adjacency matrix above

# COLONY 17
# AGGRESSION ASSAY
# WEIGHTED
# TUBE NEST
Colony17TubeAggnNetwork <- graph_from_adjacency_matrix(Colony17TubeAggnMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony17CircleAggnNetwork <- graph_from_adjacency_matrix(Colony17CircleAggnMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# UNWEIGHTED
# TUBE NEST
# Changing the matrix cells to 1s and 0s (interaction / no interaction)
Colony17TubeAggnUnweighted <- ifelse(Colony17TubeAggnMatrix > 1, 1, Colony17TubeAggnMatrix)

# CIRCLE NEST
Colony17CircleAggnUnweighted <- ifelse(Colony17CircleAggnMatrix > 1, 1, Colony17CircleAggnMatrix)

# NETWORK FROM ADJACENCY MATRICES
# TUBE NEST
Colony17TubeAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony17TubeAggnUnweighted) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony17CircleAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony17CircleAggnUnweighted) # Make a network from adjacency matrix above

# BASELINE ASSAY
# WEIGHTED
# TUBE NEST
Colony17TubePreNetwork <- graph_from_adjacency_matrix(Colony17TubePreMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony17CirclePreNetwork <- graph_from_adjacency_matrix(Colony17CirclePreMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# UNWEIGHTED
# TUBE NEST
# Changing the matrix cells to 1s and 0s (interaction / no interaction)
Colony17TubePreUnweighted <- ifelse(Colony17TubePreMatrix > 1, 1, Colony17TubePreMatrix)

# CIRCLE NEST
Colony17CirclePreUnweighted <- ifelse(Colony17CirclePreMatrix > 1, 1, Colony17CirclePreMatrix)

# NETWORK FROM ADJACENCY MATRICES
# TUBE NEST
Colony17TubePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony17TubePreUnweighted) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony17CirclePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony17CirclePreUnweighted) # Make a network from adjacency matrix above

# COLONY 18
# AGGRESSION ASSAY
# WEIGHTED
# TUBE NEST
Colony18TubeAggnNetwork <- graph_from_adjacency_matrix(Colony18TubeAggnMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony18CircleAggnNetwork <- graph_from_adjacency_matrix(Colony18CircleAggnMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# UNWEIGHTED
# TUBE NEST
# Changing the matrix cells to 1s and 0s (interaction / no interaction)
Colony18TubeAggnUnweighted <- ifelse(Colony18TubeAggnMatrix > 1, 1, Colony18TubeAggnMatrix)

# CIRCLE NEST
Colony18CircleAggnUnweighted <- ifelse(Colony18CircleAggnMatrix > 1, 1, Colony18CircleAggnMatrix)

# NETWORK FROM ADJACENCY MATRICES
# TUBE NEST
Colony18TubeAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony18TubeAggnUnweighted) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony18CircleAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony18CircleAggnUnweighted) # Make a network from adjacency matrix above

# BASELINE ASSAY
# WEIGHTED
# TUBE NEST
Colony18TubePreNetwork <- graph_from_adjacency_matrix(Colony18TubePreMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony18CirclePreNetwork <- graph_from_adjacency_matrix(Colony18CirclePreMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# UNWEIGHTED
# TUBE NEST
# Changing the matrix cells to 1s and 0s (interaction / no interaction)
Colony18TubePreUnweighted <- ifelse(Colony18TubePreMatrix > 1, 1, Colony18TubePreMatrix)

# CIRCLE NEST
Colony18CirclePreUnweighted <- ifelse(Colony18CirclePreMatrix > 1, 1, Colony18CirclePreMatrix)

# NETWORK FROM ADJACENCY MATRICES
# TUBE NEST
Colony18TubePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony18TubePreUnweighted) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony18CirclePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony18CirclePreUnweighted) # Make a network from adjacency matrix above

# COLONY 20
# AGGRESSION ASSAY
# WEIGHTED
# TUBE NEST
Colony20TubeAggnNetwork <- graph_from_adjacency_matrix(Colony20TubeAggnMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony20CircleAggnNetwork <- graph_from_adjacency_matrix(Colony20CircleAggnMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# UNWEIGHTED
# TUBE NEST
# Changing the matrix cells to 1s and 0s (interaction / no interaction)
Colony20TubeAggnUnweighted <- ifelse(Colony20TubeAggnMatrix > 1, 1, Colony20TubeAggnMatrix)

# CIRCLE NEST
Colony20CircleAggnUnweighted <- ifelse(Colony20CircleAggnMatrix > 1, 1, Colony20CircleAggnMatrix)

# NETWORK FROM ADJACENCY MATRICES
# TUBE NEST
Colony20TubeAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony20TubeAggnUnweighted) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony20CircleAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony20CircleAggnUnweighted) # Make a network from adjacency matrix above

# BASELINE ASSAY
# WEIGHTED
# TUBE NEST
Colony20TubePreNetwork <- graph_from_adjacency_matrix(Colony20TubePreMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony20CirclePreNetwork <- graph_from_adjacency_matrix(Colony20CirclePreMatrix, weighted = TRUE) # Make a network from adjacency matrix above

# UNWEIGHTED
# TUBE NEST
# Changing the matrix cells to 1s and 0s (interaction / no interaction)
Colony20TubePreUnweighted <- ifelse(Colony20TubePreMatrix > 1, 1, Colony20TubePreMatrix)

# CIRCLE NEST
Colony20CirclePreUnweighted <- ifelse(Colony20TubePreMatrix > 1, 1, Colony20TubePreMatrix)

# NETWORK FROM ADJACENCY MATRICES
# TUBE NEST
Colony20TubePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony20TubePreUnweighted) # Make a network from adjacency matrix above

# CIRCLE NEST
Colony20CirclePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony20CirclePreUnweighted) # Make a network from adjacency matrix above

##########################################################################################################
# CALCULATE NETWORK EFFICIENCY AND HARMONIC CENTRALITY
# The purpose of this section is to generate dataframes composed of (1) network efficiency or (2) Node Harmonic Centrality, and then combine them for analyses
##########################################################################################################

# COLONY 5
# TUBE NEST
# AGGRESSION ASSAY
# Number of edges in the matrix
n <- gorder(Colony5TubeAggnNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony5TubeAggnNetworkUnweighted)

# Network degree
d <- degree(Colony5TubeAggnNetworkUnweighted)

# Harmonic centrality 
HC <- harmonic_centrality(Colony5TubeAggnNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony5TubeAggnHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony5TubeAggnHC <- rownames_to_column(Colony5TubeAggnHC, "ID") 

# Read in distance to entrance dataset
Colony5TubeAggnDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony5TubeAggnDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony5TubeAggnDist1 <- Colony5TubeAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony5TubeAggnDist1$ID <- interaction("id", Colony5TubeAggnDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony5TubeAggnHCDist <- left_join(Colony5TubeAggnHC, Colony5TubeAggnDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony5TubeAggnHCDistFinal <- as.data.frame(lapply(Colony5TubeAggnHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony5TubeAggnNetworkUnweighted, v = V(Colony5TubeAggnNetworkUnweighted), to = V(Colony5TubeAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)
 
# Creating a data table for the network efficiency value
Colony5TubeAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 5, Nest = "Tube", Trial = "Aggn")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony5TubeAggnNetworkUnweighted)

# Transitivity
c <- transitivity(Colony5TubeAggnNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony5TubeAggnComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 5, Nest = "Tube", Trial = "Aggn")

# BASELINE ASSAY
# Number of edges in the matrix
n <- gorder(Colony5TubePreNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony5TubePreNetworkUnweighted)

# Network degree
d <- degree(Colony5TubePreNetworkUnweighted)

# Harmonic centrality
HC <- harmonic_centrality(Colony5TubePreNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony5TubePreHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony5TubePreHC <- rownames_to_column(Colony5TubePreHC, "ID") 

# Read in distance to entrance dataset
Colony5TubePreDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony5TubePreDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony5TubePreDist1 <- Colony5TubePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony5TubePreDist1$ID <- interaction("id", Colony5TubePreDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony5TubePreHCDist <- left_join(Colony5TubePreHC, Colony5TubePreDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony5TubePreHCDistFinal <- as.data.frame(lapply(Colony5TubePreHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# Joining the aggression and baseline assay distance to the nest entrance, harmonic centrality datasets
Colony5TubeHCDistFinal <- full_join(Colony5TubeAggnHCDistFinal, Colony5TubePreHCDistFinal)

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony5TubePreNetworkUnweighted, v = V(Colony5TubePreNetworkUnweighted), to = V(Colony5TubePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony5TubePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 5, Nest = "Tube", Trial = "Pre")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony5TubePreNetworkUnweighted)

# Transitivity
c <- transitivity(Colony5TubePreNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony5TubePreComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 5, Nest = "Tube", Trial = "Pre")

# CIRCLE NEST
# AGGRESSION ASSAY
# Number of edges in the matrix
n <- gorder(Colony5CircleAggnNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony5CircleAggnNetworkUnweighted)

# Network degree
d <- degree(Colony5CircleAggnNetworkUnweighted)

# Harmonic centrality 
HC <- harmonic_centrality(Colony5CircleAggnNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony5CircleAggnHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony5CircleAggnHC <- rownames_to_column(Colony5CircleAggnHC, "ID") 

# Read in distance to entrance dataset
Colony5CircleAggnDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony5CircleAggnDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony5CircleAggnDist1 <- Colony5CircleAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony5CircleAggnDist1$ID <- interaction("id", Colony5CircleAggnDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony5CircleAggnHCDist <- left_join(Colony5CircleAggnHC, Colony5CircleAggnDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony5CircleAggnHCDistFinal <- as.data.frame(lapply(Colony5CircleAggnHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony5CircleAggnNetworkUnweighted, v = V(Colony5CircleAggnNetworkUnweighted), to = V(Colony5CircleAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony5CircleAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 5, Nest = "Circle", Trial = "Aggn")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony5CircleAggnNetworkUnweighted)

# Transitivity
c <- transitivity(Colony5CircleAggnNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony5CircleAggnComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 5, Nest = "Circle", Trial = "Aggn")

# BASELINE ASSAY
# Number of edges in the matrix
n <- gorder(Colony5CirclePreNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony5CirclePreNetworkUnweighted)

# Network degree
d <- degree(Colony5CirclePreNetworkUnweighted)

# Harmonic centrality
HC <- harmonic_centrality(Colony5CirclePreNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony5CirclePreHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony5CirclePreHC <- rownames_to_column(Colony5CirclePreHC, "ID") 

# Read in distance to entrance dataset
Colony5CirclePreDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony5CirclePreDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony5CirclePreDist1 <- Colony5CirclePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony5CirclePreDist1$ID <- interaction("id", Colony5CirclePreDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony5CirclePreHCDist <- left_join(Colony5CirclePreHC, Colony5CirclePreDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony5CirclePreHCDistFinal <- as.data.frame(lapply(Colony5CirclePreHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# Joining the aggression and baseline assay distance to the nest entrance, harmonic centrality datasets
Colony5CircleHCDistFinal <- full_join(Colony5CircleAggnHCDistFinal, Colony5CirclePreHCDistFinal)

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony5CirclePreNetworkUnweighted, v = V(Colony5CirclePreNetworkUnweighted), to = V(Colony5CirclePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony5CirclePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 5, Nest = "Circle", Trial = "Pre")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony5CirclePreNetworkUnweighted)

# Transitivity
c <- transitivity(Colony5CirclePreNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony5CirclePreComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 5, Nest = "Circle", Trial = "Pre")

# FULL DATASETS
# Harmonic centrality
Colony5HCDistFinal <- full_join(Colony5TubeHCDistFinal, Colony5CircleHCDistFinal)

# Network efficiency
Colony5NetEfficiency <- full_join(Colony5TubeAggnEfficiency, Colony5TubePreEfficiency) %>%
  full_join(Colony5CircleAggnEfficiency) %>%
  full_join(Colony5CirclePreEfficiency)

# Comparative measures
Colony5NetComparison <- full_join(Colony5TubeAggnComp, Colony5TubePreComp) %>%
  full_join(Colony5CircleAggnComp) %>%
  full_join(Colony5CirclePreComp)

# COLONY 6
# TUBE NEST
# AGGRESSION ASSAY
# Number of edges in the matrix
n <- gorder(Colony6TubeAggnNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony6TubeAggnNetworkUnweighted)

# Network degree
d <- degree(Colony6TubeAggnNetworkUnweighted)

# Harmonic centrality 
HC <- harmonic_centrality(Colony6TubeAggnNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony6TubeAggnHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony6TubeAggnHC <- rownames_to_column(Colony6TubeAggnHC, "ID") 

# Read in distance to entrance dataset
Colony6TubeAggnDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony6TubeAggnDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony6TubeAggnDist1 <- Colony6TubeAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony6TubeAggnDist1$ID <- interaction("id", Colony6TubeAggnDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony6TubeAggnHCDist <- left_join(Colony6TubeAggnHC, Colony6TubeAggnDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony6TubeAggnHCDistFinal <- as.data.frame(lapply(Colony6TubeAggnHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony6TubeAggnNetworkUnweighted, v = V(Colony6TubeAggnNetworkUnweighted), to = V(Colony6TubeAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony6TubeAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 6, Nest = "Tube", Trial = "Aggn")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony6TubeAggnNetworkUnweighted)

# Transitivity
c <- transitivity(Colony6TubeAggnNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony6TubeAggnComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 6, Nest = "Tube", Trial = "Aggn")

# BASELINE ASSAY
# Number of edges in the matrix
n <- gorder(Colony6TubePreNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony6TubePreNetworkUnweighted)

# Network degree
d <- degree(Colony6TubePreNetworkUnweighted)

# Harmonic centrality
HC <- harmonic_centrality(Colony6TubePreNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony6TubePreHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony6TubePreHC <- rownames_to_column(Colony6TubePreHC, "ID") 

# Read in distance to entrance dataset
Colony6TubePreDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony6TubePreDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony6TubePreDist1 <- Colony6TubePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony6TubePreDist1$ID <- interaction("id", Colony6TubePreDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony6TubePreHCDist <- left_join(Colony6TubePreHC, Colony6TubePreDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony6TubePreHCDistFinal <- as.data.frame(lapply(Colony6TubePreHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# Joining the aggression and baseline assay distance to the nest entrance, harmonic centrality datasets
Colony6TubeHCDistFinal <- full_join(Colony6TubeAggnHCDistFinal, Colony6TubePreHCDistFinal)

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony6TubePreNetworkUnweighted, v = V(Colony6TubePreNetworkUnweighted), to = V(Colony6TubePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony6TubePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 6, Nest = "Tube", Trial = "Pre")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony6TubePreNetworkUnweighted)

# Transitivity
c <- transitivity(Colony6TubePreNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony6TubePreComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 6, Nest = "Tube", Trial = "Pre")

# CIRCLE NEST
# AGGRESSION ASSAY
# Number of edges in the matrix
n <- gorder(Colony6CircleAggnNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony6CircleAggnNetworkUnweighted)

# Network degree
d <- degree(Colony6CircleAggnNetworkUnweighted)

# Harmonic centrality 
HC <- harmonic_centrality(Colony6CircleAggnNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony6CircleAggnHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony6CircleAggnHC <- rownames_to_column(Colony6CircleAggnHC, "ID") 

# Read in distance to entrance dataset
Colony6CircleAggnDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony6CircleAggnDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony6CircleAggnDist1 <- Colony6CircleAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony6CircleAggnDist1$ID <- interaction("id", Colony6CircleAggnDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony6CircleAggnHCDist <- left_join(Colony6CircleAggnHC, Colony6CircleAggnDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony6CircleAggnHCDistFinal <- as.data.frame(lapply(Colony6CircleAggnHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony6CircleAggnNetworkUnweighted, v = V(Colony6CircleAggnNetworkUnweighted), to = V(Colony6CircleAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony6CircleAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 6, Nest = "Circle", Trial = "Aggn")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony6CircleAggnNetworkUnweighted)

# Transitivity
c <- transitivity(Colony6CircleAggnNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony6CircleAggnComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 6, Nest = "Circle", Trial = "Aggn")

# BASELINE ASSAY
# Number of edges in the matrix
n <- gorder(Colony6CirclePreNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony6CirclePreNetworkUnweighted)

# Network degree
d <- degree(Colony6CirclePreNetworkUnweighted)


# Harmonic centrality
HC <- harmonic_centrality(Colony6CirclePreNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony6CirclePreHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony6CirclePreHC <- rownames_to_column(Colony6CirclePreHC, "ID") 

# Read in distance to entrance dataset
Colony6CirclePreDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony6CirclePreDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony6CirclePreDist1 <- Colony6CirclePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony6CirclePreDist1$ID <- interaction("id", Colony6CirclePreDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony6CirclePreHCDist <- left_join(Colony6CirclePreHC, Colony6CirclePreDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony6CirclePreHCDistFinal <- as.data.frame(lapply(Colony6CirclePreHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# Joining the aggression and baseline assay distance to the nest entrance, harmonic centrality datasets
Colony6CircleHCDistFinal <- full_join(Colony6CircleAggnHCDistFinal, Colony6CirclePreHCDistFinal)

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony6CirclePreNetworkUnweighted, v = V(Colony6CirclePreNetworkUnweighted), to = V(Colony6CirclePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony6CirclePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 6, Nest = "Circle", Trial = "Pre")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony6CirclePreNetworkUnweighted)

# Transitivity
c <- transitivity(Colony6CirclePreNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony6CirclePreComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 6, Nest = "Circle", Trial = "Pre")

# FULL DATASET
# Harmonic centrality
Colony6HCDistFinal <- full_join(Colony6TubeHCDistFinal, Colony6CircleHCDistFinal)

# Network efficiency
Colony6NetEfficiency <- full_join(Colony6TubeAggnEfficiency, Colony6TubePreEfficiency) %>%
  full_join(Colony6CircleAggnEfficiency) %>%
  full_join(Colony6CirclePreEfficiency)

# Comparative measures
Colony6NetComparison <- full_join(Colony6TubeAggnComp, Colony6TubePreComp) %>%
  full_join(Colony6CircleAggnComp) %>%
  full_join(Colony6CirclePreComp)

# COLONY 7
# TUBE NEST
# AGGRESSION ASSAY
# Number of edges in the matrix
n <- gorder(Colony7TubeAggnNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony7TubeAggnNetworkUnweighted)

# Network degree
d <- degree(Colony7TubeAggnNetworkUnweighted)

# Harmonic centrality 
HC <- harmonic_centrality(Colony7TubeAggnNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony7TubeAggnHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony7TubeAggnHC <- rownames_to_column(Colony7TubeAggnHC, "ID") 

# Read in distance to entrance dataset
Colony7TubeAggnDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony7TubeAggnDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony7TubeAggnDist1 <- Colony7TubeAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony7TubeAggnDist1$ID <- interaction("id", Colony7TubeAggnDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony7TubeAggnHCDist <- left_join(Colony7TubeAggnHC, Colony7TubeAggnDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony7TubeAggnHCDistFinal <- as.data.frame(lapply(Colony7TubeAggnHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony7TubeAggnNetworkUnweighted, v = V(Colony7TubeAggnNetworkUnweighted), to = V(Colony7TubeAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony7TubeAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 7, Nest = "Tube", Trial = "Aggn")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony7TubeAggnNetworkUnweighted)

# Transitivity
c <- transitivity(Colony7TubeAggnNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony7TubeAggnComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 7, Nest = "Tube", Trial = "Aggn")

# BASELINE ASSAY
# Number of edges in the matrix
n <- gorder(Colony7TubePreNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony7TubePreNetworkUnweighted)

# Network degree
d <- degree(Colony7TubePreNetworkUnweighted)

# Harmonic centrality
HC <- harmonic_centrality(Colony7TubePreNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony7TubePreHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony7TubePreHC <- rownames_to_column(Colony7TubePreHC, "ID") 

# Read in distance to entrance dataset
Colony7TubePreDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony7TubePreDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony7TubePreDist1 <- Colony7TubePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony7TubePreDist1$ID <- interaction("id", Colony7TubePreDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony7TubePreHCDist <- left_join(Colony7TubePreHC, Colony7TubePreDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony7TubePreHCDistFinal <- as.data.frame(lapply(Colony7TubePreHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# Joining the aggression and baseline assay distance to the nest entrance, harmonic centrality datasets
Colony7TubeHCDistFinal <- full_join(Colony7TubeAggnHCDistFinal, Colony7TubePreHCDistFinal)

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony7TubePreNetworkUnweighted, v = V(Colony7TubePreNetworkUnweighted), to = V(Colony7TubePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony7TubePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 7, Nest = "Tube", Trial = "Pre")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony7TubePreNetworkUnweighted)

# Transitivity
c <- transitivity(Colony7TubePreNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony7TubePreComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 7, Nest = "Tube", Trial = "Pre")

# CIRCLE NEST
# AGGRESSION ASSAY
# Number of edges in the matrix
n <- gorder(Colony7CircleAggnNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony7CircleAggnNetworkUnweighted)

# Network degree
d <- degree(Colony7CircleAggnNetworkUnweighted)

# Harmonic centrality 
HC <- harmonic_centrality(Colony7CircleAggnNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony7CircleAggnHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony7CircleAggnHC <- rownames_to_column(Colony7CircleAggnHC, "ID") 

# Read in distance to entrance dataset
Colony7CircleAggnDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony7CircleAggnDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony7CircleAggnDist1 <- Colony7CircleAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony7CircleAggnDist1$ID <- interaction("id", Colony7CircleAggnDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony7CircleAggnHCDist <- left_join(Colony7CircleAggnHC, Colony7CircleAggnDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony7CircleAggnHCDistFinal <- as.data.frame(lapply(Colony7CircleAggnHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony7CircleAggnNetworkUnweighted, v = V(Colony7CircleAggnNetworkUnweighted), to = V(Colony7CircleAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony7CircleAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 7, Nest = "Circle", Trial = "Aggn")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony7CircleAggnNetworkUnweighted)

# Transitivity
c <- transitivity(Colony7CircleAggnNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony7CircleAggnComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 7, Nest = "Circle", Trial = "Aggn")

# BASELINE ASSAY
# Number of edges in the matrix
n <- gorder(Colony7CirclePreNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony7CirclePreNetworkUnweighted)

# Network degree
d <- degree(Colony7CirclePreNetworkUnweighted)

# Harmonic centrality
HC <- harmonic_centrality(Colony7CirclePreNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony7CirclePreHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony7CirclePreHC <- rownames_to_column(Colony7CirclePreHC, "ID") 

# Read in distance to entrance dataset
Colony7CirclePreDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony7CirclePreDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony7CirclePreDist1 <- Colony7CirclePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony7CirclePreDist1$ID <- interaction("id", Colony7CirclePreDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony7CirclePreHCDist <- left_join(Colony7CirclePreHC, Colony7CirclePreDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony7CirclePreHCDistFinal <- as.data.frame(lapply(Colony7CirclePreHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# Joining the aggression and baseline assay distance to the nest entrance, harmonic centrality datasets
Colony7CircleHCDistFinal <- full_join(Colony7CircleAggnHCDistFinal, Colony7CirclePreHCDistFinal)

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony7CirclePreNetworkUnweighted, v = V(Colony7CirclePreNetworkUnweighted), to = V(Colony7CirclePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony7CirclePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 7, Nest = "Circle", Trial = "Pre")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony7CirclePreNetworkUnweighted)

# Transitivity
c <- transitivity(Colony7CirclePreNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony7CirclePreComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 7, Nest = "Circle", Trial = "Pre")

# FULL DATASET
Colony7HCDistFinal <- full_join(Colony7TubeHCDistFinal, Colony7CircleHCDistFinal)

# Network efficiency
Colony7NetEfficiency <- full_join(Colony7TubeAggnEfficiency, Colony7TubePreEfficiency) %>%
  full_join(Colony7CircleAggnEfficiency) %>%
  full_join(Colony7CirclePreEfficiency)

# Comparative measures
Colony7NetComparison <- full_join(Colony7TubeAggnComp, Colony7TubePreComp) %>%
  full_join(Colony7CircleAggnComp) %>%
  full_join(Colony7CirclePreComp)

# COLONY 8
# TUBE NEST
# AGGRESSION ASSAY
# Number of edges in the matrix
n <- gorder(Colony8TubeAggnNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony8TubeAggnNetworkUnweighted)

# Network degree
d <- degree(Colony8TubeAggnNetworkUnweighted)

# Harmonic centrality 
HC <- harmonic_centrality(Colony8TubeAggnNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony8TubeAggnHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony8TubeAggnHC <- rownames_to_column(Colony8TubeAggnHC, "ID") 

# Read in distance to entrance dataset
Colony8TubeAggnDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony8TubeAggnDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony8TubeAggnDist1 <- Colony8TubeAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony8TubeAggnDist1$ID <- interaction("id", Colony8TubeAggnDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony8TubeAggnHCDist <- left_join(Colony8TubeAggnHC, Colony8TubeAggnDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony8TubeAggnHCDistFinal <- as.data.frame(lapply(Colony8TubeAggnHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony8TubeAggnNetworkUnweighted, v = V(Colony8TubeAggnNetworkUnweighted), to = V(Colony8TubeAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony8TubeAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 8, Nest = "Tube", Trial = "Aggn")

# Comparative MEASURES
# GLOBAL

# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony8TubeAggnNetworkUnweighted)

# Transitivity
c <- transitivity(Colony8TubeAggnNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony8TubeAggnComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 8, Nest = "Tube", Trial = "Aggn")

# BASELINE ASSAY
# Number of edges in the matrix
n <- gorder(Colony8TubePreNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony8TubePreNetworkUnweighted)

# Network degree
d <- degree(Colony8TubePreNetworkUnweighted)

# Harmonic centrality
HC <- harmonic_centrality(Colony8TubePreNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony8TubePreHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony8TubePreHC <- rownames_to_column(Colony8TubePreHC, "ID") 

# Read in distance to entrance dataset
Colony8TubePreDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony8TubePreDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony8TubePreDist1 <- Colony8TubePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony8TubePreDist1$ID <- interaction("id", Colony8TubePreDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony8TubePreHCDist <- left_join(Colony8TubePreHC, Colony8TubePreDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony8TubePreHCDistFinal <- as.data.frame(lapply(Colony8TubePreHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# Joining the aggression and baseline assay distance to the nest entrance, harmonic centrality datasets
Colony8TubeHCDistFinal <- full_join(Colony8TubeAggnHCDistFinal, Colony8TubePreHCDistFinal)

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony8TubePreNetworkUnweighted, v = V(Colony8TubePreNetworkUnweighted), to = V(Colony8TubePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony8TubePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 8, Nest = "Tube", Trial = "Pre")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony8TubePreNetworkUnweighted)

# Transitivity
c <- transitivity(Colony8TubePreNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony8TubePreComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 8, Nest = "Tube", Trial = "Pre")

# CIRCLE NEST
# AGGRESSION ASSAY
# Number of edges in the matrix
n <- gorder(Colony8CircleAggnNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony8CircleAggnNetworkUnweighted)

# Network degree
d <- degree(Colony8CircleAggnNetworkUnweighted)

# Harmonic centrality 
HC <- harmonic_centrality(Colony8CircleAggnNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony8CircleAggnHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony8CircleAggnHC <- rownames_to_column(Colony8CircleAggnHC, "ID") 

# Read in distance to entrance dataset
Colony8CircleAggnDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony8CircleAggnDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony8CircleAggnDist1 <- Colony8CircleAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony8CircleAggnDist1$ID <- interaction("id", Colony8CircleAggnDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony8CircleAggnHCDist <- left_join(Colony8CircleAggnHC, Colony8CircleAggnDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony8CircleAggnHCDistFinal <- as.data.frame(lapply(Colony8CircleAggnHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony8CircleAggnNetworkUnweighted, v = V(Colony8CircleAggnNetworkUnweighted), to = V(Colony8CircleAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony8CircleAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 8, Nest = "Circle", Trial = "Aggn")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony8CircleAggnNetworkUnweighted)

# Transitivity
c <- transitivity(Colony8CircleAggnNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony8CircleAggnComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 8, Nest = "Circle", Trial = "Aggn")

# BASELINE ASSAY
# Number of edges in the matrix
n <- gorder(Colony8CirclePreNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony8CirclePreNetworkUnweighted)

# Network degree
d <- degree(Colony8CirclePreNetworkUnweighted)

# Harmonic centrality
HC <- harmonic_centrality(Colony8CirclePreNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony8CirclePreHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony8CirclePreHC <- rownames_to_column(Colony8CirclePreHC, "ID") 

# Read in distance to entrance dataset
Colony8CirclePreDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony8CirclePreDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony8CirclePreDist1 <- Colony8CirclePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony8CirclePreDist1$ID <- interaction("id", Colony8CirclePreDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony8CirclePreHCDist <- left_join(Colony8CirclePreHC, Colony8CirclePreDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony8CirclePreHCDistFinal <- as.data.frame(lapply(Colony8CirclePreHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# Joining the aggression and baseline assay distance to the nest entrance, harmonic centrality datasets
Colony8CircleHCDistFinal <- full_join(Colony8CircleAggnHCDistFinal, Colony8CirclePreHCDistFinal)

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony8CirclePreNetworkUnweighted, v = V(Colony8CirclePreNetworkUnweighted), to = V(Colony8CirclePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony8CirclePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 8, Nest = "Circle", Trial = "Pre")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony8CirclePreNetworkUnweighted)

# Transitivity
c <- transitivity(Colony8CirclePreNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony8CirclePreComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 8, Nest = "Circle", Trial = "Pre")

# FULL DATASET
# Harmonic centrality
Colony8HCDistFinal <- full_join(Colony8TubeHCDistFinal, Colony8CircleHCDistFinal)

# Network efficiency
Colony8NetEfficiency <- full_join(Colony8TubeAggnEfficiency, Colony8TubePreEfficiency) %>%
  full_join(Colony8CircleAggnEfficiency) %>%
  full_join(Colony8CirclePreEfficiency)

# Comparative measures
Colony8NetComparison <- full_join(Colony8TubeAggnComp, Colony8TubePreComp) %>%
  full_join(Colony8CircleAggnComp) %>%
  full_join(Colony8CirclePreComp)

# COLONY 9
# TUBE NEST
# AGGRESSION ASSAY
# Number of edges in the matrix
n <- gorder(Colony9TubeAggnNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony9TubeAggnNetworkUnweighted)

# Network degree
d <- degree(Colony9TubeAggnNetworkUnweighted)

# Harmonic centrality 
HC <- harmonic_centrality(Colony9TubeAggnNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony9TubeAggnHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony9TubeAggnHC <- rownames_to_column(Colony9TubeAggnHC, "ID") 

# Read in distance to entrance dataset
Colony9TubeAggnDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony9TubeAggnDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony9TubeAggnDist1 <- Colony9TubeAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony9TubeAggnDist1$ID <- interaction("id", Colony9TubeAggnDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony9TubeAggnHCDist <- left_join(Colony9TubeAggnHC, Colony9TubeAggnDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony9TubeAggnHCDistFinal <- as.data.frame(lapply(Colony9TubeAggnHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony9TubeAggnNetworkUnweighted, v = V(Colony9TubeAggnNetworkUnweighted), to = V(Colony9TubeAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony9TubeAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 9, Nest = "Tube", Trial = "Aggn")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony9TubeAggnNetworkUnweighted)

# Transitivity
c <- transitivity(Colony9TubeAggnNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony9TubeAggnComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 9, Nest = "Tube", Trial = "Aggn")

# BASELINE ASSAY
# Number of edges in the matrix
n <- gorder(Colony9TubePreNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony9TubePreNetworkUnweighted)

# Network degree
d <- degree(Colony9TubePreNetworkUnweighted)

# Harmonic centrality
HC <- harmonic_centrality(Colony9TubePreNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony9TubePreHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony9TubePreHC <- rownames_to_column(Colony9TubePreHC, "ID") 

# Read in distance to entrance dataset
Colony9TubePreDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony9TubePreDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony9TubePreDist1 <- Colony9TubePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony9TubePreDist1$ID <- interaction("id", Colony9TubePreDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony9TubePreHCDist <- left_join(Colony9TubePreHC, Colony9TubePreDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony9TubePreHCDistFinal <- as.data.frame(lapply(Colony9TubePreHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# Joining the aggression and baseline assay distance to the nest entrance, harmonic centrality datasets
Colony9TubeHCDistFinal <- full_join(Colony9TubeAggnHCDistFinal, Colony9TubePreHCDistFinal)

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony9TubePreNetworkUnweighted, v = V(Colony9TubePreNetworkUnweighted), to = V(Colony9TubePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony9TubePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 9, Nest = "Tube", Trial = "Pre")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony9TubePreNetworkUnweighted)

# Transitivity
c <- transitivity(Colony9TubePreNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony9TubePreComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 9, Nest = "Tube", Trial = "Pre")

# CIRCLE NEST
# AGGRESSION ASSAY
# Number of edges in the matrix
n <- gorder(Colony9CircleAggnNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony9CircleAggnNetworkUnweighted)

# Network degree
d <- degree(Colony9CircleAggnNetworkUnweighted)

# Harmonic centrality 
HC <- harmonic_centrality(Colony9CircleAggnNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony9CircleAggnHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony9CircleAggnHC <- rownames_to_column(Colony9CircleAggnHC, "ID") 

# Read in distance to entrance dataset
Colony9CircleAggnDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony9CircleAggnDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony9CircleAggnDist1 <- Colony9CircleAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony9CircleAggnDist1$ID <- interaction("id", Colony9CircleAggnDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony9CircleAggnHCDist <- left_join(Colony9CircleAggnHC, Colony9CircleAggnDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony9CircleAggnHCDistFinal <- as.data.frame(lapply(Colony9CircleAggnHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony9CircleAggnNetworkUnweighted, v = V(Colony9CircleAggnNetworkUnweighted), to = V(Colony9CircleAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony9CircleAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 9, Nest = "Circle", Trial = "Aggn")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony9CircleAggnNetworkUnweighted)

# Transitivity
c <- transitivity(Colony9CircleAggnNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony9CircleAggnComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 9, Nest = "Circle", Trial = "Aggn")

# BASELINE ASSAY
# Number of edges in the matrix
n <- gorder(Colony9CirclePreNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony9CirclePreNetworkUnweighted)

# Network degree
d <- degree(Colony9CirclePreNetworkUnweighted)

# Harmonic centrality
HC <- harmonic_centrality(Colony9CirclePreNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony9CirclePreHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony9CirclePreHC <- rownames_to_column(Colony9CirclePreHC, "ID") 

# Read in distance to entrance dataset
Colony9CirclePreDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony9CirclePreDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony9CirclePreDist1 <- Colony9CirclePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony9CirclePreDist1$ID <- interaction("id", Colony9CirclePreDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony9CirclePreHCDist <- left_join(Colony9CirclePreHC, Colony9CirclePreDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony9CirclePreHCDistFinal <- as.data.frame(lapply(Colony9CirclePreHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# Joining the aggression and baseline assay distance to the nest entrance, harmonic centrality datasets
Colony9CircleHCDistFinal <- full_join(Colony9CircleAggnHCDistFinal, Colony9CirclePreHCDistFinal)

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony9CirclePreNetworkUnweighted, v = V(Colony9CirclePreNetworkUnweighted), to = V(Colony9CirclePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony9CirclePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 9, Nest = "Circle", Trial = "Pre")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony9CirclePreNetworkUnweighted)

# Transitivity
c <- transitivity(Colony9CirclePreNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony9CirclePreComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 9, Nest = "Circle", Trial = "Pre")

# FULL DATASET
# Harmonic centrality
Colony9HCDistFinal <- full_join(Colony9TubeHCDistFinal, Colony9CircleHCDistFinal)

# Network efficiency
Colony9NetEfficiency <- full_join(Colony9TubeAggnEfficiency, Colony9TubePreEfficiency) %>%
  full_join(Colony9CircleAggnEfficiency) %>%
  full_join(Colony9CirclePreEfficiency)

# Comparative measures
Colony9NetComparison <- full_join(Colony9TubeAggnComp, Colony9TubePreComp) %>%
  full_join(Colony9CircleAggnComp) %>%
  full_join(Colony9CirclePreComp)

# COLONY 11
# TUBE NEST
# AGGRESSION ASSAY
# Number of edges in the matrix
n <- gorder(Colony11TubeAggnNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony11TubeAggnNetworkUnweighted)

# Network degree
d <- degree(Colony11TubeAggnNetworkUnweighted)

# Harmonic centrality 
HC <- harmonic_centrality(Colony11TubeAggnNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony11TubeAggnHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony11TubeAggnHC <- rownames_to_column(Colony11TubeAggnHC, "ID") 

# Read in distance to entrance dataset
Colony11TubeAggnDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony11TubeAggnDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony11TubeAggnDist1 <- Colony11TubeAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony11TubeAggnDist1$ID <- interaction("id", Colony11TubeAggnDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony11TubeAggnHCDist <- left_join(Colony11TubeAggnHC, Colony11TubeAggnDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony11TubeAggnHCDistFinal <- as.data.frame(lapply(Colony11TubeAggnHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony11TubeAggnNetworkUnweighted, v = V(Colony11TubeAggnNetworkUnweighted), to = V(Colony11TubeAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony11TubeAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 11, Nest = "Tube", Trial = "Aggn")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony11TubeAggnNetworkUnweighted)

# Transitivity
c <- transitivity(Colony11TubeAggnNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony11TubeAggnComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 11, Nest = "Tube", Trial = "Aggn")

# BASELINE ASSAY
# Number of edges in the matrix
n <- gorder(Colony11TubePreNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony11TubePreNetworkUnweighted)

# Network degree
d <- degree(Colony11TubePreNetworkUnweighted)

# Harmonic centrality
HC <- harmonic_centrality(Colony11TubePreNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony11TubePreHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony11TubePreHC <- rownames_to_column(Colony11TubePreHC, "ID") 

# Read in distance to entrance dataset
Colony11TubePreDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony11TubePreDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony11TubePreDist1 <- Colony11TubePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony11TubePreDist1$ID <- interaction("id", Colony11TubePreDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony11TubePreHCDist <- left_join(Colony11TubePreHC, Colony11TubePreDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony11TubePreHCDistFinal <- as.data.frame(lapply(Colony11TubePreHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# Joining the aggression and baseline assay distance to the nest entrance, harmonic centrality datasets
Colony11TubeHCDistFinal <- full_join(Colony11TubeAggnHCDistFinal, Colony11TubePreHCDistFinal)

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony11TubePreNetworkUnweighted, v = V(Colony11TubePreNetworkUnweighted), to = V(Colony11TubePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony11TubePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 11, Nest = "Tube", Trial = "Pre")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony11TubePreNetworkUnweighted)

# Transitivity
c <- transitivity(Colony11TubePreNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony11TubePreComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 11, Nest = "Tube", Trial = "Pre")

# CIRCLE NEST
# AGGRESSION ASSAY
# Number of edges in the matrix
n <- gorder(Colony11CircleAggnNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony11CircleAggnNetworkUnweighted)

# Network degree
d <- degree(Colony11CircleAggnNetworkUnweighted)

# Harmonic centrality 
HC <- harmonic_centrality(Colony11CircleAggnNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony11CircleAggnHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony11CircleAggnHC <- rownames_to_column(Colony11CircleAggnHC, "ID") 

# Read in distance to entrance dataset
Colony11CircleAggnDist <- read.csv("https://data.cyverse.org/dav-anon/iplant/home/gchism/NestArchAggn/Derived_Data/distance_entrance/Colony11CircleAggnDist.csv")

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony11CircleAggnDist1 <- Colony11CircleAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony11CircleAggnDist1$ID <- interaction("id", Colony11CircleAggnDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony11CircleAggnHCDist <- left_join(Colony11CircleAggnHC, Colony11CircleAggnDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony11CircleAggnHCDistFinal <- as.data.frame(lapply(Colony11CircleAggnHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony11CircleAggnNetworkUnweighted, v = V(Colony11CircleAggnNetworkUnweighted), to = V(Colony11CircleAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony11CircleAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 11, Nest = "Circle", Trial = "Aggn")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony11CircleAggnNetworkUnweighted)

# Transitivity
c <- transitivity(Colony11CircleAggnNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony11CircleAggnComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 11, Nest = "Circle", Trial = "Aggn")

# BASELINE ASSAY
# Number of edges in the matrix
n <- gorder(Colony11CirclePreNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony11CirclePreNetworkUnweighted)

# Network degree
d <- degree(Colony11CirclePreNetworkUnweighted)

# Harmonic centrality
HC <- harmonic_centrality(Colony11CirclePreNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony11CirclePreHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony11CirclePreHC <- rownames_to_column(Colony11CirclePreHC, "ID") 

# Read in distance to entrance dataset
Colony11CirclePreDist <- read.csv("https://data.cyverse.org/dav-anon/iplant/home/gchism/NestArchAggn/Derived_Data/distance_entrance/Colony11CirclePreDist.csv")

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony11CirclePreDist1 <- Colony11CirclePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony11CirclePreDist1$ID <- interaction("id", Colony11CirclePreDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony11CirclePreHCDist <- left_join(Colony11CirclePreHC, Colony11CirclePreDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony11CirclePreHCDistFinal <- as.data.frame(lapply(Colony11CirclePreHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# Joining the aggression and baseline assay distance to the nest entrance, harmonic centrality datasets
Colony11CircleHCDistFinal <- full_join(Colony11CircleAggnHCDistFinal, Colony11CirclePreHCDistFinal)

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony11CirclePreNetworkUnweighted, v = V(Colony11CirclePreNetworkUnweighted), to = V(Colony11CirclePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony11CirclePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 11, Nest = "Circle", Trial = "Pre")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony11CirclePreNetworkUnweighted)

# Transitivity
c <- transitivity(Colony11CirclePreNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony11CirclePreComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 11, Nest = "Circle", Trial = "Pre")

# FULL DATASET
# Harmonic centrality
Colony11HCDistFinal <- full_join(Colony11TubeHCDistFinal, Colony11CircleHCDistFinal)

# Network efficiency
Colony11NetEfficiency <- full_join(Colony11TubeAggnEfficiency, Colony11TubePreEfficiency) %>%
  full_join(Colony11CircleAggnEfficiency) %>%
  full_join(Colony11CirclePreEfficiency)

# Comparative measures
Colony11NetComparison <- full_join(Colony11TubeAggnComp, Colony11TubePreComp) %>%
  full_join(Colony11CircleAggnComp) %>%
  full_join(Colony11CirclePreComp)

# COLONY 13
# TUBE NEST
# AGGRESSION ASSAY
# Number of edges in the matrix
n <- gorder(Colony13TubeAggnNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony13TubeAggnNetworkUnweighted)

# Network degree
d <- degree(Colony13TubeAggnNetworkUnweighted)

# Harmonic centrality 
HC <- harmonic_centrality(Colony13TubeAggnNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony13TubeAggnHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony13TubeAggnHC <- rownames_to_column(Colony13TubeAggnHC, "ID") 

# Read in distance to entrance dataset
Colony13TubeAggnDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony13TubeAggnDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony13TubeAggnDist1 <- Colony13TubeAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony13TubeAggnDist1$ID <- interaction("id", Colony13TubeAggnDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony13TubeAggnHCDist <- left_join(Colony13TubeAggnHC, Colony13TubeAggnDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony13TubeAggnHCDistFinal <- as.data.frame(lapply(Colony13TubeAggnHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony13TubeAggnNetworkUnweighted, v = V(Colony13TubeAggnNetworkUnweighted), to = V(Colony13TubeAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony13TubeAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 13, Nest = "Tube", Trial = "Aggn")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony13TubeAggnNetworkUnweighted)

# Transitivity
c <- transitivity(Colony13TubeAggnNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony13TubeAggnComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 13, Nest = "Tube", Trial = "Aggn")

# BASELINE ASSAY
# Number of edges in the matrix
n <- gorder(Colony13TubePreNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony13TubePreNetworkUnweighted)

# Network degree
d <- degree(Colony13TubePreNetworkUnweighted)

# Harmonic centrality
HC <- harmonic_centrality(Colony13TubePreNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony13TubePreHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony13TubePreHC <- rownames_to_column(Colony13TubePreHC, "ID") 

# Read in distance to entrance dataset
Colony13TubePreDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony13TubePreDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony13TubePreDist1 <- Colony13TubePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony13TubePreDist1$ID <- interaction("id", Colony13TubePreDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony13TubePreHCDist <- left_join(Colony13TubePreHC, Colony13TubePreDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony13TubePreHCDistFinal <- as.data.frame(lapply(Colony13TubePreHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# Joining the aggression and baseline assay distance to the nest entrance, harmonic centrality datasets
Colony13TubeHCDistFinal <- full_join(Colony13TubeAggnHCDistFinal, Colony13TubePreHCDistFinal)

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony13TubePreNetworkUnweighted, v = V(Colony13TubePreNetworkUnweighted), to = V(Colony13TubePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony13TubePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 13, Nest = "Tube", Trial = "Pre")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony13TubePreNetworkUnweighted)

# Transitivity
c <- transitivity(Colony13TubePreNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony13TubePreComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 13, Nest = "Tube", Trial = "Pre")

# CIRCLE NEST
# AGGRESSION ASSAY
# Number of edges in the matrix
n <- gorder(Colony13CircleAggnNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony13CircleAggnNetworkUnweighted)

# Network degree
d <- degree(Colony13CircleAggnNetworkUnweighted)

# Harmonic centrality 
HC <- harmonic_centrality(Colony13CircleAggnNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony13CircleAggnHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony13CircleAggnHC <- rownames_to_column(Colony13CircleAggnHC, "ID") 

# Read in distance to entrance dataset
Colony13CircleAggnDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony13CircleAggnDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony13CircleAggnDist1 <- Colony13CircleAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony13CircleAggnDist1$ID <- interaction("id", Colony13CircleAggnDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony13CircleAggnHCDist <- left_join(Colony13CircleAggnHC, Colony13CircleAggnDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony13CircleAggnHCDistFinal <- as.data.frame(lapply(Colony13CircleAggnHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony13CircleAggnNetworkUnweighted, v = V(Colony13CircleAggnNetworkUnweighted), to = V(Colony13CircleAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony13CircleAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 13, Nest = "Circle", Trial = "Aggn")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony13CircleAggnNetworkUnweighted)

# Transitivity
c <- transitivity(Colony13CircleAggnNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony13CircleAggnComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 13, Nest = "Circle", Trial = "Aggn")

# BASELINE ASSAY
# Number of edges in the matrix
n <- gorder(Colony13CirclePreNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony13CirclePreNetworkUnweighted)

# Network degree
d <- degree(Colony13CirclePreNetworkUnweighted)

# Harmonic centrality
HC <- harmonic_centrality(Colony13CirclePreNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony13CirclePreHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony13CirclePreHC <- rownames_to_column(Colony13CirclePreHC, "ID") 

# Read in distance to entrance dataset
Colony13CirclePreDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony13CirclePreDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony13CirclePreDist1 <- Colony13CirclePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony13CirclePreDist1$ID <- interaction("id", Colony13CirclePreDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony13CirclePreHCDist <- left_join(Colony13CirclePreHC, Colony13CirclePreDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony13CirclePreHCDistFinal <- as.data.frame(lapply(Colony13CirclePreHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# Joining the aggression and baseline assay distance to the nest entrance, harmonic centrality datasets
Colony13CircleHCDistFinal <- full_join(Colony13CircleAggnHCDistFinal, Colony13CirclePreHCDistFinal)

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony13CirclePreNetworkUnweighted, v = V(Colony13CirclePreNetworkUnweighted), to = V(Colony13CirclePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony13CirclePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 13, Nest = "Circle", Trial = "Pre")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony13CirclePreNetworkUnweighted)

# Transitivity
c <- transitivity(Colony13CirclePreNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony13CirclePreComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 13, Nest = "Circle", Trial = "Pre")

# FULL DATASET
# Harmonic centrality
Colony13HCDistFinal <- full_join(Colony13TubeHCDistFinal, Colony13CircleHCDistFinal)

# Network efficiency
Colony13NetEfficiency <- full_join(Colony13TubeAggnEfficiency, Colony13TubePreEfficiency) %>%
  full_join(Colony13CircleAggnEfficiency) %>%
  full_join(Colony13CirclePreEfficiency)

# Comparative measures
Colony13NetComparison <- full_join(Colony13TubeAggnComp, Colony13TubePreComp) %>%
  full_join(Colony13CircleAggnComp) %>%
  full_join(Colony13CirclePreComp)

# COLONY 17
# TUBE NEST
# AGGRESSION ASSAY
# Number of edges in the matrix
n <- gorder(Colony17TubeAggnNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony17TubeAggnNetworkUnweighted)

# Network degree
d <- degree(Colony17TubeAggnNetworkUnweighted)

# Harmonic centrality 
HC <- harmonic_centrality(Colony17TubeAggnNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony17TubeAggnHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony17TubeAggnHC <- rownames_to_column(Colony17TubeAggnHC, "ID") 

# Read in distance to entrance dataset
Colony17TubeAggnDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony17TubeAggnDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony17TubeAggnDist1 <- Colony17TubeAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony17TubeAggnDist1$ID <- interaction("id", Colony17TubeAggnDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony17TubeAggnHCDist <- left_join(Colony17TubeAggnHC, Colony17TubeAggnDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony17TubeAggnHCDistFinal <- as.data.frame(lapply(Colony17TubeAggnHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony17TubeAggnNetworkUnweighted, v = V(Colony17TubeAggnNetworkUnweighted), to = V(Colony17TubeAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony17TubeAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 17, Nest = "Tube", Trial = "Aggn")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony17TubeAggnNetworkUnweighted)

# Transitivity
c <- transitivity(Colony17TubeAggnNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony17TubeAggnComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 17, Nest = "Tube", Trial = "Aggn")

# BASELINE ASSAY
# Number of edges in the matrix
n <- gorder(Colony17TubePreNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony17TubePreNetworkUnweighted)

# Network degree
d <- degree(Colony17TubePreNetworkUnweighted)

# Harmonic centrality
HC <- harmonic_centrality(Colony17TubePreNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony17TubePreHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony17TubePreHC <- rownames_to_column(Colony17TubePreHC, "ID") 

# Read in distance to entrance dataset
Colony17TubePreDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony17TubePreDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony17TubePreDist1 <- Colony17TubePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony17TubePreDist1$ID <- interaction("id", Colony17TubePreDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony17TubePreHCDist <- left_join(Colony17TubePreHC, Colony17TubePreDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony17TubePreHCDistFinal <- as.data.frame(lapply(Colony17TubePreHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# Joining the aggression and baseline assay distance to the nest entrance, harmonic centrality datasets
Colony17TubeHCDistFinal <- full_join(Colony17TubeAggnHCDistFinal, Colony17TubePreHCDistFinal)

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony17TubePreNetworkUnweighted, v = V(Colony17TubePreNetworkUnweighted), to = V(Colony17TubePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony17TubePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 17, Nest = "Tube", Trial = "Pre")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony17TubePreNetworkUnweighted)

# Transitivity
c <- transitivity(Colony17TubePreNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony17TubePreComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 17, Nest = "Tube", Trial = "Pre")

# CIRCLE NEST
# AGGRESSION ASSAY
# Number of edges in the matrix
n <- gorder(Colony17CircleAggnNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony17CircleAggnNetworkUnweighted)

# Network degree
d <- degree(Colony17CircleAggnNetworkUnweighted)

# Harmonic centrality 
HC <- harmonic_centrality(Colony17CircleAggnNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony17CircleAggnHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony17CircleAggnHC <- rownames_to_column(Colony17CircleAggnHC, "ID") 

# Read in distance to entrance dataset
Colony17CircleAggnDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony17CircleAggnDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony17CircleAggnDist1 <- Colony17CircleAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony17CircleAggnDist1$ID <- interaction("id", Colony17CircleAggnDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony17CircleAggnHCDist <- left_join(Colony17CircleAggnHC, Colony17CircleAggnDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony17CircleAggnHCDistFinal <- as.data.frame(lapply(Colony17CircleAggnHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony17CircleAggnNetworkUnweighted, v = V(Colony17CircleAggnNetworkUnweighted), to = V(Colony17CircleAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony17CircleAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 17, Nest = "Circle", Trial = "Aggn")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony17CircleAggnNetworkUnweighted)

# Transitivity
c <- transitivity(Colony17CircleAggnNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony17CircleAggnComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 17, Nest = "Circle", Trial = "Aggn")

# BASELINE ASSAY
# Number of edges in the matrix
n <- gorder(Colony17CirclePreNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony17CirclePreNetworkUnweighted)

# Network degree
d <- degree(Colony17CirclePreNetworkUnweighted)

# Harmonic centrality
HC <- harmonic_centrality(Colony17CirclePreNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony17CirclePreHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony17CirclePreHC <- rownames_to_column(Colony17CirclePreHC, "ID") 

# Read in distance to entrance dataset
Colony17CirclePreDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony17CirclePreDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony17CirclePreDist1 <- Colony17CirclePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony17CirclePreDist1$ID <- interaction("id", Colony17CirclePreDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony17CirclePreHCDist <- left_join(Colony17CirclePreHC, Colony17CirclePreDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony17CirclePreHCDistFinal <- as.data.frame(lapply(Colony17CirclePreHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# Joining the aggression and baseline assay distance to the nest entrance, harmonic centrality datasets
Colony17CircleHCDistFinal <- full_join(Colony17CircleAggnHCDistFinal, Colony17CirclePreHCDistFinal)

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony17CirclePreNetworkUnweighted, v = V(Colony17CirclePreNetworkUnweighted), to = V(Colony17CirclePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony17CirclePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 17, Nest = "Circle", Trial = "Pre")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony17CirclePreNetworkUnweighted)

# Transitivity
c <- transitivity(Colony17CirclePreNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony17CirclePreComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 17, Nest = "Circle", Trial = "Pre")

# FULL DATASET
# Harmonic centrality
Colony17HCDistFinal <- full_join(Colony17TubeHCDistFinal, Colony17CircleHCDistFinal)

# Network efficiency
Colony17NetEfficiency <- full_join(Colony17TubeAggnEfficiency, Colony17TubePreEfficiency) %>%
  full_join(Colony17CircleAggnEfficiency) %>%
  full_join(Colony17CirclePreEfficiency)

# Comparative measures
Colony17NetComparison <- full_join(Colony17TubeAggnComp, Colony17TubePreComp) %>%
  full_join(Colony17CircleAggnComp) %>%
  full_join(Colony17CirclePreComp)

# COLONY 18
# TUBE NEST
# AGGRESSION ASSAY
# Number of edges in the matrix
n <- gorder(Colony18TubeAggnNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony18TubeAggnNetworkUnweighted)

# Network degree
d <- degree(Colony18TubeAggnNetworkUnweighted)

# Harmonic centrality 
HC <- harmonic_centrality(Colony18TubeAggnNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony18TubeAggnHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony18TubeAggnHC <- rownames_to_column(Colony18TubeAggnHC, "ID") 

# Read in distance to entrance dataset
Colony18TubeAggnDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony18TubeAggnDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony18TubeAggnDist1 <- Colony18TubeAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony18TubeAggnDist1$ID <- interaction("id", Colony18TubeAggnDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony18TubeAggnHCDist <- left_join(Colony18TubeAggnHC, Colony18TubeAggnDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony18TubeAggnHCDistFinal <- as.data.frame(lapply(Colony18TubeAggnHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony18TubeAggnNetworkUnweighted, v = V(Colony18TubeAggnNetworkUnweighted), to = V(Colony18TubeAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony18TubeAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 18, Nest = "Tube", Trial = "Aggn")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony18TubeAggnNetworkUnweighted)

# Transitivity
c <- transitivity(Colony18TubeAggnNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony18TubeAggnComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 18, Nest = "Tube", Trial = "Aggn")

# BASELINE ASSAY
# Number of edges in the matrix
n <- gorder(Colony18TubePreNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony18TubePreNetworkUnweighted)

# Network degree
d <- degree(Colony18TubePreNetworkUnweighted)

# Harmonic centrality
HC <- harmonic_centrality(Colony18TubePreNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony18TubePreHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony18TubePreHC <- rownames_to_column(Colony18TubePreHC, "ID") 

# Read in distance to entrance dataset
Colony18TubePreDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony18TubePreDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony18TubePreDist1 <- Colony18TubePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony18TubePreDist1$ID <- interaction("id", Colony18TubePreDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony18TubePreHCDist <- left_join(Colony18TubePreHC, Colony18TubePreDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony18TubePreHCDistFinal <- as.data.frame(lapply(Colony18TubePreHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# Joining the aggression and baseline assay distance to the nest entrance, harmonic centrality datasets
Colony18TubeHCDistFinal <- full_join(Colony18TubeAggnHCDistFinal, Colony18TubePreHCDistFinal)

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony18TubePreNetworkUnweighted, v = V(Colony18TubePreNetworkUnweighted), to = V(Colony18TubePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony18TubePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 18, Nest = "Tube", Trial = "Pre")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony18TubePreNetworkUnweighted)

# Transitivity
c <- transitivity(Colony18TubePreNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony18TubePreComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 18, Nest = "Tube", Trial = "Pre")

# CIRCLE NEST
# AGGRESSION ASSAY
# Number of edges in the matrix
n <- gorder(Colony18CircleAggnNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony18CircleAggnNetworkUnweighted)

# Network degree
d <- degree(Colony18CircleAggnNetworkUnweighted)

# Harmonic centrality 
HC <- harmonic_centrality(Colony18CircleAggnNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony18CircleAggnHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony18CircleAggnHC <- rownames_to_column(Colony18CircleAggnHC, "ID") 

# Read in distance to entrance dataset
Colony18CircleAggnDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony18CircleAggnDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony18CircleAggnDist1 <- Colony18CircleAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony18CircleAggnDist1$ID <- interaction("id", Colony18CircleAggnDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony18CircleAggnHCDist <- left_join(Colony18CircleAggnHC, Colony18CircleAggnDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony18CircleAggnHCDistFinal <- as.data.frame(lapply(Colony18CircleAggnHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony18CircleAggnNetworkUnweighted, v = V(Colony18CircleAggnNetworkUnweighted), to = V(Colony18CircleAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony18CircleAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 18, Nest = "Circle", Trial = "Aggn")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony18CircleAggnNetworkUnweighted)

# Transitivity
c <- transitivity(Colony18CircleAggnNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony18CircleAggnComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 18, Nest = "Circle", Trial = "Aggn")

# BASELINE ASSAY
# Number of edges in the matrix
n <- gorder(Colony18CirclePreNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony18CirclePreNetworkUnweighted)

# Network degree
d <- degree(Colony18CirclePreNetworkUnweighted)

# Harmonic centrality
HC <- harmonic_centrality(Colony18CirclePreNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony18CirclePreHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony18CirclePreHC <- rownames_to_column(Colony18CirclePreHC, "ID") 

# Read in distance to entrance dataset
Colony18CirclePreDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony18CirclePreDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony18CirclePreDist1 <- Colony18CirclePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony18CirclePreDist1$ID <- interaction("id", Colony18CirclePreDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony18CirclePreHCDist <- left_join(Colony18CirclePreHC, Colony18CirclePreDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony18CirclePreHCDistFinal <- as.data.frame(lapply(Colony18CirclePreHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# Joining the aggression and baseline assay distance to the nest entrance, harmonic centrality datasets
Colony18CircleHCDistFinal <- full_join(Colony18CircleAggnHCDistFinal, Colony18CirclePreHCDistFinal)

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony18CirclePreNetworkUnweighted, v = V(Colony18CirclePreNetworkUnweighted), to = V(Colony18CirclePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony18CirclePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 18, Nest = "Circle", Trial = "Pre")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony18CirclePreNetworkUnweighted)

# Transitivity
c <- transitivity(Colony18CirclePreNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony18CirclePreComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 18, Nest = "Circle", Trial = "Pre")

# FULL DATASET
# Harmonic centrality
Colony18HCDistFinal <- full_join(Colony18TubeHCDistFinal, Colony18CircleHCDistFinal)

# Network efficiency
Colony18NetEfficiency <- full_join(Colony18TubeAggnEfficiency, Colony18TubePreEfficiency) %>%
  full_join(Colony18CircleAggnEfficiency) %>%
  full_join(Colony18CirclePreEfficiency)

# Comparative measures
Colony18NetComparison <- full_join(Colony18TubeAggnComp, Colony18TubePreComp) %>%
  full_join(Colony18CircleAggnComp) %>%
  full_join(Colony18CirclePreComp)

# COLONY 20
# TUBE NEST
# AGGRESSION ASSAY
# Number of edges in the matrix
n <- gorder(Colony20TubeAggnNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony20TubeAggnNetworkUnweighted)

# Network degree
d <- degree(Colony20TubeAggnNetworkUnweighted)

# Harmonic centrality 
HC <- harmonic_centrality(Colony20TubeAggnNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony20TubeAggnHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony20TubeAggnHC <- rownames_to_column(Colony20TubeAggnHC, "ID") 

# Read in distance to entrance dataset
Colony20TubeAggnDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony20TubeAggnDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony20TubeAggnDist1 <- Colony20TubeAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony20TubeAggnDist1$ID <- interaction("id", Colony20TubeAggnDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony20TubeAggnHCDist <- left_join(Colony20TubeAggnHC, Colony20TubeAggnDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony20TubeAggnHCDistFinal <- as.data.frame(lapply(Colony20TubeAggnHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony20TubeAggnNetworkUnweighted, v = V(Colony20TubeAggnNetworkUnweighted), to = V(Colony20TubeAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony20TubeAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 20, Nest = "Tube", Trial = "Aggn")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony20TubeAggnNetworkUnweighted)

# Transitivity
c <- transitivity(Colony20TubeAggnNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony20TubeAggnComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 20, Nest = "Tube", Trial = "Aggn")

# BASELINE ASSAY
# Number of edges in the matrix
n <- gorder(Colony20TubePreNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony20TubePreNetworkUnweighted)

# Network degree
d <- degree(Colony20TubePreNetworkUnweighted)

# Harmonic centrality
HC <- harmonic_centrality(Colony20TubePreNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony20TubePreHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony20TubePreHC <- rownames_to_column(Colony20TubePreHC, "ID") 

# Read in distance to entrance dataset
Colony20TubePreDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony20TubePreDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony20TubePreDist1 <- Colony20TubePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony20TubePreDist1$ID <- interaction("id", Colony20TubePreDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony20TubePreHCDist <- left_join(Colony20TubePreHC, Colony20TubePreDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony20TubePreHCDistFinal <- as.data.frame(lapply(Colony20TubePreHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# Joining the aggression and baseline assay distance to the nest entrance, harmonic centrality datasets
Colony20TubeHCDistFinal <- full_join(Colony20TubeAggnHCDistFinal, Colony20TubePreHCDistFinal)

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony20TubePreNetworkUnweighted, v = V(Colony20TubePreNetworkUnweighted), to = V(Colony20TubePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony20TubePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 20, Nest = "Tube", Trial = "Pre")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony20TubePreNetworkUnweighted)

# Transitivity
c <- transitivity(Colony20TubePreNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony20TubePreComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 20, Nest = "Tube", Trial = "Pre")

# CIRCLE NEST
# AGGRESSION ASSAY
# Number of edges in the matrix
n <- gorder(Colony20CircleAggnNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony20CircleAggnNetworkUnweighted)

# Network degree
d <- degree(Colony20CircleAggnNetworkUnweighted)

# Harmonic centrality 
HC <- harmonic_centrality(Colony20CircleAggnNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony20CircleAggnHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony20CircleAggnHC <- rownames_to_column(Colony20CircleAggnHC, "ID") 

# Read in distance to entrance dataset
Colony20CircleAggnDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony20CircleAggnDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony20CircleAggnDist1 <- Colony20CircleAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony20CircleAggnDist1$ID <- interaction("id", Colony20CircleAggnDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony20CircleAggnHCDist <- left_join(Colony20CircleAggnHC, Colony20CircleAggnDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony20CircleAggnHCDistFinal <- as.data.frame(lapply(Colony20CircleAggnHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony20CircleAggnNetworkUnweighted, v = V(Colony20CircleAggnNetworkUnweighted), to = V(Colony20CircleAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony20CircleAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 20, Nest = "Circle", Trial = "Aggn")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony20CircleAggnNetworkUnweighted)

# Transitivity
c <- transitivity(Colony20CircleAggnNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony20CircleAggnComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 20, Nest = "Circle", Trial = "Aggn")

# BASELINE ASSAY
# Number of edges in the matrix
n <- gorder(Colony20CirclePreNetworkUnweighted)

# Number of nodes in the matrix
m <- gsize(Colony20CirclePreNetworkUnweighted)

# Network degree
d <- degree(Colony20CirclePreNetworkUnweighted)

# Harmonic centrality
HC <- harmonic_centrality(Colony20CirclePreNetworkUnweighted, mode = c("all"))

# Create a data set from the HC vector
Colony20CirclePreHC <- as.data.frame(cbind(HC))

# Making the column names the row names
Colony20CirclePreHC <- rownames_to_column(Colony20CirclePreHC, "ID") 

# Read in distance to entrance dataset
Colony20CirclePreDist <- read.csv(here("analysis", "data", "derived_data", "distance_entrance", "Colony20CirclePreDist.csv"))

# Taking the distance to the nest entrance dataset and filtering out only data from the very beginning of the video
Colony20CirclePreDist1 <- Colony20CirclePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>% dplyr::select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

# Changing the ID column to match the networks 
Colony20CirclePreDist1$ID <- interaction("id", Colony20CirclePreDist1$ID, sep = "_")

# Left join the distance to the nest entrance dataset to the harmonic centrality dataset, and drop NAs
Colony20CirclePreHCDist <- left_join(Colony20CirclePreHC, Colony20CirclePreDist1) %>% drop_na()

# Create a new dataset that scales the values of harmonic centrality by max possible connections that a node can have
Colony20CirclePreHCDistFinal <- as.data.frame(lapply(Colony20CirclePreHCDist, unlist)) %>% mutate(ScaledHC = HC / (n - 1))

# Joining the aggression and baseline assay distance to the nest entrance, harmonic centrality datasets
Colony20CircleHCDistFinal <- full_join(Colony20CircleAggnHCDistFinal, Colony20CirclePreHCDistFinal)

# NETWORK EFFICIENCY
# Calculate the shortest distance (in edges) across the network for each node
shortest_path_1 <- distances(Colony20CirclePreNetworkUnweighted, v = V(Colony20CirclePreNetworkUnweighted), to = V(Colony20CirclePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

# Reciprocal of shortest paths
a <- 1 / shortest_path_1

# Diagonals should be NAs
diag(a) = NA

# Global efficiency calculation
Efficiency <- ((sum(a, na.rm = TRUE) * (1 / (n * (n - 1)))) / ((n * (n - 1))) / 2)

# Creating a data table for the network efficiency value
Colony20CirclePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 20, Nest = "Circle", Trial = "Pre")

# Comparative MEASURES
# Gamma connectivity
Gam <- (m / (3 * (n - 2)))

# Diameter
net_diam <- max(s(shortest_path_1)) # Diameter of the network

# Reciprocity
r <- reciprocity(Colony20CirclePreNetworkUnweighted)

# Transitivity
c <- transitivity(Colony20CirclePreNetworkUnweighted, type = "global") # Clustering for undirected

# Combine Comparative measures
Colony20CirclePreComp <- data.frame(GamConn = Gam, Diam = net_diam, Recip = r, Trans = c) %>% mutate(Colony = 20, Nest = "Circle", Trial = "Pre")

# FULL DATASET
# Harmonic centrality
Colony20HCDistFinal <- full_join(Colony20TubeHCDistFinal, Colony20CircleHCDistFinal)

# Network efficiency
Colony20NetEfficiency <- full_join(Colony20TubeAggnEfficiency, Colony20TubePreEfficiency) %>%
  full_join(Colony20CircleAggnEfficiency) %>%
  full_join(Colony20CirclePreEfficiency)

# Comparative measures
Colony20NetComparison <- full_join(Colony20TubeAggnComp, Colony20TubePreComp) %>%
  full_join(Colony20CircleAggnComp) %>%
  full_join(Colony20CirclePreComp)

# FINAL COMBINED DATASETS  
# Harmonic centrality
FullHCDistFinal <- full_join(Colony5HCDistFinal, Colony6HCDistFinal) %>%
  full_join(Colony7HCDistFinal) %>%
  full_join(Colony8HCDistFinal) %>%
  full_join(Colony9HCDistFinal) %>%
  full_join(Colony11HCDistFinal) %>%
  full_join(Colony13HCDistFinal) %>%
  full_join(Colony17HCDistFinal) %>%
  full_join(Colony18HCDistFinal) %>%
  full_join(Colony20HCDistFinal) 

# Network efficiency
FullNetEfficiencyFinal <- full_join(Colony5NetEfficiency, Colony6NetEfficiency) %>%
  full_join(Colony7NetEfficiency) %>%
  full_join(Colony8NetEfficiency) %>%
  full_join(Colony9NetEfficiency) %>%
  full_join(Colony11NetEfficiency) %>%
  full_join(Colony13NetEfficiency) %>%
  full_join(Colony17NetEfficiency) %>%
  full_join(Colony18NetEfficiency) %>%
  full_join(Colony20NetEfficiency) 

# Comparative measures
FullNetComparisonFinal <- full_join(Colony5NetComparison, Colony6NetComparison) %>%
  full_join(Colony7NetComparison) %>%
  full_join(Colony8NetComparison) %>%
  full_join(Colony9NetComparison) %>%
  full_join(Colony11NetComparison) %>%
  full_join(Colony13NetComparison) %>%
  full_join(Colony17NetComparison) %>%
  full_join(Colony18NetComparison) %>%
  full_join(Colony20NetComparison)

##########################################################################################################
# ANALYSES
# Analyses for harmonic centrality, network efficiency, and all comparative network measures
##########################################################################################################

# HARMONIC CENTRALITY

# LINEAR REGRESSION
# RESPONSE VARIABLE
# HC - Harmonic centrality of networks
# FIXED EFFECTS
# ScaledDist - Worker scaled distances from the nest entrance (0 - 1, where 1 is the longest, shortest distance from the nest entrance)
# Nest - Nest shape (Tube / Circle)
# Trial - The assay of the video (Aggn / Pre), note that pre is the baseline assay
# Colony - Colony identification 
tab_model(lm(HC ~ ScaledDist * Nest * Trial + Colony, data = FullHCDistFinal), 
          show.stat = T)

# NETWORK EFFICIENCY

# LINEAR REGRESSION
# RESPONSE VARIABLE
# log(Efficiency) - Network efficiency, log transformed because numbers are very small
# FIXED EFFECTS
# Nest - Nest shape (Tube / Circle)
# Trial - The assay of the video (Aggn / Pre), note that pre is the baseline assay
# Colony - Colony identification 
tab_model(lm(log(Efficiency) ~ Nest * Trial + Colony, data = FullNetEfficiencyFinal),
          show.stat = T)

# NETWORK COMPARISONS

# GAMMA CONNECTIVITY
# LINEAR REGRESSION
# RESPONSE VARIABLE
# GamConn - Gamma connectivity of the network
# FIXED EFFECTS
# Nest - Nest shape (Tube / Circle)
# Trial - The assay of the video (Aggn / Pre), note that pre is the baseline assay
# Colony - Colony identification 
tab_model(lm(GamConn ~ Nest * Trial + Colony, data = FullNetComparisonFinal),
          show.stat = T)

# NETWORK DIAMETER
# LINEAR REGRESSION
# RESPONSE VARIABLE
# Diam - Diameter of the network
# FIXED EFFECTS
# Nest - Nest shape (Tube / Circle)
# Trial - The assay of the video (Aggn / Pre), note that pre is the baseline assay
# Colony - Colony identification 
tab_model(lm(Diam ~ Nest * Trial + Colony, data = FullNetComparisonFinal),
          show.stat = T)

# NETWORK RECIPROCITY
# LINEAR REGRESSION
# RESPONSE VARIABLE
# Recip - Reciprocity of the network
# FIXED EFFECTS
# Nest - Nest shape (Tube / Circle)
# Trial - The assay of the video (Aggn / Pre), note that pre is the baseline assay
# Colony - Colony identification 
tab_model(lm(Recip ~ Nest * Trial + Colony, data = FullNetComparisonFinal),
          show.stat = T)

# NETWORK TRANSITIVITY
# LINEAR REGRESSION
# RESPONSE VARIABLE
# Trans - Transitivity of the network
# FIXED EFFECTS
# Nest - Nest shape (Tube / Circle)
# Trial - The assay of the video (Aggn / Pre), note that pre is the baseline assay
# Colony - Colony identification 
tab_model(lm(Trans ~ Nest * Trial + Colony, data = FullNetComparisonFinal),
          show.stat = T)

##########################################################################################################
# PLOTS
# Plots for all of the network analyses, and network visualizations
##########################################################################################################

# Function to create large points in a geom_point legend
large_points <- function(data, params, size) {
  # Multiply by some number, it doesn't matter what value, but larger numbers = large sized points in the legend
  data$size <- data$size * 2.5
  draw_key_point(data = data, params = params, size = size)
}

# HARMONIC CENTRALITY
# TUBE NEST
# AGGRESSION ASSAY
TubeAggnHC
TubeAggnHC <- FullHCDistFinal %>% 
  filter(Nest == "Tube" & Trial == "Aggn") %>%
  ggplot(aes(x = ScaledDist, y = ScaledHC)) +
  ggtitle("Tube Invader") +
  geom_point(key_glyph = large_points, size = 2.5, alpha = 0.75, color = "red") +
  geom_smooth(method = 'lm', se = FALSE, size = 1.5, color = "black") +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_text(size = 18, color = "white"),
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 20, color = "black", hjust = 0.1),
        legend.key = element_blank()) +
  xlab(NULL) +
  ylab(NULL)

# BASELINE ASSAY  
TubePreHC <- FullHCDistFinal %>% 
  filter(Nest == "Tube" & Trial == "Pre") %>%
  ggplot(aes(x = ScaledDist, y = ScaledHC)) +
    ggtitle("Tube Baseline") +
    geom_point(key_glyph = large_points, size = 2.5, alpha = 0.75, color = "red") +
    geom_smooth(method = 'lm', se = FALSE, size = 1.5, color = "black") +
    theme_pubclean() +
    theme(axis.ticks = element_blank(),
          axis.text.y = element_text(size= 18, color = "white"),
          axis.text.x = element_text(size = 18, color = "black"),
          axis.title = element_blank(),
          plot.title = element_text(size = 20, color = "black", hjust = 0.1),
          legend.key = element_blank()) +
    xlab(NULL) +
    ylab(NULL) 

# CIRCLE NEST
# AGGRESSION ASSAY
CircleAggnHC <- FullHCDistFinal %>% 
  filter(Nest == "Circle" & Trial == "Aggn") %>%
  ggplot(aes(x = ScaledDist, y = ScaledHC)) +
    ggtitle("Circle Invader") +
    geom_point(key_glyph = large_points, size = 2.5, alpha = 0.75, color = "blue") +
    geom_smooth(method = 'lm', se = FALSE, size = 1.5, color = "black") +
    theme_pubclean() +
    theme(axis.ticks = element_blank(),
          axis.text.y = element_text(size = 18, color ="black"),
          axis.text.x = element_text(size = 18, color = "black"),
          axis.title = element_blank(),
          plot.title = element_text(size = 20, color = "black", hjust = 0.1),
          legend.key = element_blank()) +
    xlab(NULL) +
    ylab(NULL) +
    ylim(0, 1)
  
# BASELINE ASSAY
CirclePreHC <- FullHCDistFinal %>% 
  filter(Nest == "Circle" & Trial == "Pre") %>%
  ggplot(aes(x = ScaledDist, y = ScaledHC)) +
    ggtitle("Circle Baseline") +
    geom_point(key_glyph = large_points, size = 2.5, alpha = 0.75, color = "blue") +
    geom_smooth(method = 'lm', se = FALSE, size = 1.5, color = "black") +
    theme_pubclean() +
    theme(axis.ticks = element_blank(),
          axis.text.y = element_text(size = 18, color = "black"),
          axis.text.x = element_text(size = 18, color = "black"),
          axis.title = element_blank(),
          plot.title = element_text(size = 20, color = "black", hjust = 0.1),
          legend.key = element_blank()) +
    xlab(NULL) +
    ylab(NULL) 

# Compiling the harmonic centrality plots  
TestHCDistPlot <- ggarrange(CirclePreHC, TubePreHC,
                          CircleAggnHC, TubeAggnHC,
                        labels = c("(a)", "(b)", "(c)", "(d)"),
                        font.label = list(size = 20, face = "plain"),
                        ncol = 2, nrow = 2)

# Annotate the compiled figure
TestHCDistPlot_Full <- annotate_figure(TestHCDistPlot,
                top = NULL,
                bottom = text_grob("Worker scaled dist entrance, frame 1", color = "black",
                                   size = 20, x = 0.53),
                left = text_grob("Harmonic centrality", color = "black",
                                 size = 20, rot = 90),
                right = NULL
)

# Save plot as a PDF
ggsave(file = here("analysis", "figures", "Fig3a_d.pdf"), plot = TestHCDistPlot_Full, width = 10.4, height = 10.4, units = "in")


# NETWORK EFFICIENCY
# BASELINE ASSAY
NetworkEfficiencyPre <- ggplot(data = FullNetEfficiencyFinal %>% filter(Trial == "Pre"), 
                               aes(y = Efficiency, x = Nest, fill = Nest)) +
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
  scale_y_log10(breaks = c(10^-6, 10^-5, 10^-4, 10^-3),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^-6, 10^-3)) +
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name = "Nest",
                    values = c("blue", "red")) + 
  annotation_logticks(sides = "l")  

# AGGRESSION ASSAY
NetworkEfficiencyAggn <- ggplot(data = FullNetEfficiencyFinal %>% filter(Trial == "Aggn"), 
                                aes(y = Efficiency, x = Nest, fill = Nest)) +
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00), alpha = 0.65) +
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
  scale_y_log10(breaks = c(10^-6, 10^-5, 10^-4, 10^-3),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^-6, 10^-3)) +
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name = "Nest",
                    values = c("blue", "red")) 

# Compiling the network efficiency plots  
NetEfficiency <- ggarrange(NetworkEfficiencyPre, NetworkEfficiencyAggn,
                          labels = c("(a)", "(b)"),
                          ncol = 2, nrow = 1,
                          font.label = list(size = 20, face = "plain"))

# Annotating the compiled plot
NetEfficiency_Full <- annotate_figure(NetEfficiency,
                top = NULL,
                bottom = text_grob("Nest shape", color = "black",
                                   size = 20, x = 0.53),
                left = text_grob("Global network efficiency", color = "black",
                                 size = 20, rot = 90),
                right = NULL
)

# Save plot as a PDF
ggsave(file = here("analysis", "figures", "Fig4.pdf"), plot = NetEfficiency_Full, width = 10.4, height = 5.2, units = "in")

# COMPARATIVE PLOTS
# GAMMA CONNECTIVITY
# BASELINE ASSAY
NetworkGamConnPre <- ggplot(data = FullNetComparisonFinal %>% filter(Trial == "Pre"), 
                            aes(y = GamConn, x = Nest, fill = Nest)) +
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
        legend.justification = c(1,1),
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        plot.title = element_text(size = 20, color = "black")) +
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name = "Nest",
                    values = c("blue", "red")) +
  ylim(0, 25)

# AGGRESSION ASSAY
NetworkGamConnAggn <- ggplot(data = FullNetComparisonFinal %>% filter(Trial == "Aggn"),
                             aes(y = GamConn, x = Nest, fill = Nest)) +
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00), alpha = 0.65) +
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
  ylim(0, 25)

# Compile the gamma connectivity plots
NetComparisonsGamma <- ggarrange(NetworkGamConnPre, NetworkGamConnAggn,
                          labels = c("(a)", "(b)"),
                          ncol = 2, nrow = 1,
                          font.label = list(size = 20, face = "plain"))

NetComparisonsGamma_Full <- annotate_figure(NetComparisonsGamma,
                top = NULL,
                bottom = text_grob("Nest shape", color = "black",
                                   size = 20, x = 0.53),
                left = text_grob("Gamma connectivity", color = "black",
                                 size = 20, rot = 90),
                right = NULL
)

# Save plot as a PDF
ggsave(file = here("analysis", "supplementary-materials", "Supplementary_Figures", "FigS4.pdf"), plot = NetComparisonsGamma_Full, width = 10.4, height = 5.2, units = "in")


# NETWORK DIAMETER
# BASELINE ASSAY
NetworkDiamPre <- ggplot(data = FullNetComparisonFinal %>% filter(Trial == "Pre"),
                         aes(y = Diam, x = Nest, fill = Nest)) +
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00), alpha = 0.65) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Baseline") +
  theme_pubclean() +
  theme(axis.text = element_text(size = 18, color = "black"),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = "none",
        legend.justification = c(1, 1),
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        plot.title = element_text(size = 20, color = "black")) +
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name = "Nest",
                    values = c("blue", "red")) +
  ylim(0, 10)

# AGGRESSION ASSAY
NetworkDiamAggn <- ggplot(data = FullNetComparisonFinal %>% filter(Trial == "Aggn"), aes(y = Diam, x = Nest, fill = Nest)) +
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00), alpha = 0.65) +
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
  ylim(0, 10)

# Compiling the network diameter plots
NetComparisonsDiam <- ggarrange(NetworkDiamPre, NetworkDiamAggn,
                               labels = c("(a)", "(b)"),
                               ncol = 2, nrow = 1,
                               font.label = list(size = 20, face = "plain"))

# Annotating the compiled plots
NetComparisonsDiam_Full <- annotate_figure(NetComparisonsDiam,
                top = NULL,
                bottom = text_grob("Nest shape", color = "black",
                                   size = 20, x = 0.53),
                left = text_grob("Diameter", color = "black",
                                 size = 20, rot = 90),
                right = NULL
)

# Save plot as a PDF (CURRENTLY NOT USING)
# ggsave(file = here("analysis", "supplementary-materials", "Supplementary_Figures", "FigS5.pdf"), plot = NetComparisonsDiam_Full, width = 10.4, height = 5.2, units = "in")

# NETWORK RECIPROCITY
# BASELINE ASSAY
NetworkRecipPre <- ggplot(data = FullNetComparisonFinal %>% filter(Trial == "Pre"),
                          aes(y = Recip, x = Nest, fill = Nest)) +
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00), alpha = 0.65) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Baseline") +
  theme_pubclean() +
  theme(axis.text = element_text(size = 18, color = "black"),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = "none",
        legend.justification = c(1, 1),
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        plot.title = element_text(size = 20, color = "black", hjust = 0.05)) +
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name = "Nest",
                    values = c("blue", "red")) +
  ylim(0.7, 1)

# AGGRESSION ASSAY
NetworkRecipAggn <- ggplot(data = FullNetComparisonFinal %>% filter(Trial == "Aggn"),
                           aes(y = Recip, x = Nest, fill = Nest)) +
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00), alpha = 0.65) +
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
        plot.title = element_text(size = 20, color = "black", hjust = 0.05)) +
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name = "Nest",
                    values = c("blue", "red")) +
  ylim(0.7, 1)

# Compiling the network reciprocity plots
NetComparisonsRecip <- ggarrange(NetworkRecipPre, NetworkRecipAggn,
                               labels = c("(a)", "(b)"),
                               ncol = 2, nrow = 1,
                               font.label = list(size = 20, face = "plain"))

# Annotate the compiled plots
NetComparisonsRecip_Full <- annotate_figure(NetComparisonsRecip,
                top = NULL,
                bottom = text_grob("Nest shape", color = "black",
                                   size = 20, x = 0.53),
                left = text_grob("Reciprocity", color = "black",
                                 size = 20, rot = 90),
                right = NULL
)

# Save plot as a PDF
ggsave(file = here("analysis", "supplementary-materials", "Supplementary_Figures", "FigS5.pdf"), plot = NetComparisonsRecip_Full, width = 10.4, height = 5.2, units = "in")

# NETWORK TRANSITIVITY
# BASELINE ASSAY
NetworkTransPre <- ggplot(data = FullNetComparisonFinal %>% filter(Trial == "Pre"),
                          aes(y = Trans, x = Nest, fill = Nest)) +
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00), alpha = 0.65) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Baseline") +
  theme_pubclean() +
  theme(axis.text = element_text(size = 18, color = "black"),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = "none",
        legend.justification = c(1, 1),
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        plot.title = element_text(size = 20, color = "black", hjust = 0.05)) +
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name = "Nest",
                    values = c("blue", "red")) +
  ylim(0.3, 1)

# AGGRESSION ASSAY
NetworkTransAggn <- ggplot(data = FullNetComparisonFinal %>% filter(Trial == "Aggn"),
                           aes(y = Trans, x = Nest, fill = Nest)) +
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00), alpha = 0.65) +
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
        plot.title = element_text(size = 20, color = "black", hjust = 0.05)) +
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name = "Nest",
                    values = c("blue", "red")) +
  ylim(0.3, 1)

# Compiling the network transitivity plots
NetComparisonsTrans <- ggarrange(NetworkTransPre, NetworkTransAggn,
                               labels = c("(a)", "(b)"),
                               ncol = 2, nrow = 1,
                               font.label = list(size = 20, face = "plain"))

# Annotate the compiled plots
NetComparisonsTrans_Full <- annotate_figure(NetComparisonsTrans,
                top = NULL,
                bottom = text_grob("Nest shape", color = "black",
                                   size = 20, x = 0.53),
                left = text_grob("Transitivity", color = "black",
                                 size = 20, rot = 90),
                right = NULL
)

# Save plot as a PDF
ggsave(file = here("analysis", "supplementary-materials", "Supplementary_Figures", "FigS6.pdf"), plot = NetComparisonsTrans_Full, width = 10.4, height = 5.2, units = "in")

# NETWORKS 
# Palette for networks
pal <- wes_palette("Zissou1", 100, type = "continuous")

# Uses the unweighted networks for colony 11 only

# CIRCLE NEST
# BASELINE ASSAY
# Number of nodes in the matrix
n <- gorder(Colony11CirclePreNetworkUnweighted)

# Harmonic centrality calculation
HC1 <- harmonic_centrality(Colony11CirclePreNetworkUnweighted, mode = c("all"))

# Scaling harmonic centrality
HC1 <- HC1 / (n - 1)

# Node properties in the plot
V(Colony11CirclePreNetworkUnweighted)$label = HC1 # Name of the nodes is the same as the number of links
V(Colony11CirclePreNetworkUnweighted)$size = HC1 # Size of the nodes are the value of harmonic centrality

# Network plot
Col11CirclePreNet <- ggraph(Colony11CirclePreNetworkUnweighted, layout = "lgl") +
  geom_edge_link(color = "gray69") +
  geom_node_point(aes(fill = HC1, size = HC1), shape = 21) +
  scale_size("Harmonic centrality", breaks = seq(0.004, 0.016, 0.002), labels = seq(0.004, 0.016, 0.002), range = c(1, 5)) +
  scale_fill_gradientn("Harmonic centrality", colours = pal, breaks = seq(0.004, 0.016, 0.002), labels = seq(0.004, 0.016, 0.002)) +
  theme_graph() +
  theme(legend.position = "none") 

# AGGRESSION ASSAY
# Number of nodes in the matrix
n <- gorder(Colony11CircleAggnNetworkUnweighted)

# Harmonic centrality calculation
HC2 <- harmonic_centrality(Colony11CircleAggnNetworkUnweighted, mode = c("all"))

# Scaling harmonic centrality
HC2 <- HC2 / (n - 1)

# Node properties in the plot
V(Colony11CircleAggnNetworkUnweighted)$label = HC2 # Name of the nodes is the same as the number of links
V(Colony11CircleAggnNetworkUnweighted)$size = HC2 # Size of the nodes are the value of harmonic centrality

# Network plot
Col11CircleAggnNet <- ggraph(Colony11CircleAggnNetworkUnweighted, layout = "lgl") +
  geom_edge_link(color = "gray69") +
  geom_node_point(aes(fill = HC2, size = HC2), shape = 21) +
  scale_size("Harmonic centrality", breaks = seq(0.002, 0.018, 0.002), labels = seq(0.002, 0.018, 0.002), range = c(1, 5)) +
  scale_fill_gradientn("Harmonic centrality", colours = pal, breaks = seq(0.002, 0.018, 0.002), labels = seq(0.002, 0.018, 0.002)) +
  theme_graph() +
  theme(legend.position = "none")

# TUBE NEST
# BASELINE ASSAY
# Number of nodes in the matrix
n <- gorder(Colony11TubePreNetworkUnweighted)

# Harmonic centrality calculation
HC3 <- harmonic_centrality(Colony11TubePreNetworkUnweighted, mode = c("all"))

# Scaling harmonic centrality
HC3 <- HC3 / (n - 1)

# Node properties in the plot
V(Colony11TubePreNetworkUnweighted)$label = HC3 # Name of the nodes is the same as the number of links
V(Colony11TubePreNetworkUnweighted)$size = HC3 # Size of the nodes are the value of harmonic centrality

# Network plot
Col11TubePreNet <- ggraph(Colony11TubePreNetworkUnweighted, layout = "lgl") +
  geom_edge_link(color = "gray69") +
  geom_node_point(aes(fill = HC3, size = HC3), shape = 21) +
  scale_size("Harmonic centrality", breaks = seq(0.004, 0.016, 0.002), labels = seq(0.004, 0.016, 0.002), range = c(1, 5)) +
  scale_fill_gradientn("Harmonic centrality", colours = pal, breaks = seq(0.004, 0.016, 0.002), labels = seq(0.004, 0.016, 0.002)) +
  theme_graph() +
  theme(legend.position = "none")

# Number of nodes in the matrix
n <- gorder(Colony11TubeAggnNetworkUnweighted)

# Harmonic centrality calculation
HC4 <- harmonic_centrality(Colony11TubeAggnNetworkUnweighted, mode = c("all"))

# Scaling harmonic centrality
HC4 <- HC4 / (n - 1)

# Node properties in the plot
V(Colony11TubeAggnNetworkUnweighted)$label = HC4 # Name of the nodes is the same as the number of links
V(Colony11TubeAggnNetworkUnweighted)$size = HC4 # Size of the nodes are the value of harmonic centrality

# AGGRESSION ASSAY
Col11TubeAggnNet <- ggraph(Colony11TubeAggnNetworkUnweighted, layout = "lgl") +
  geom_edge_link(color = "gray69") +
  geom_node_point(aes(fill = HC4, size = HC4), shape = 21) +
  scale_size("Harmonic centrality", breaks = seq(0.004, 0.016, 0.002), labels = seq(0.004, 0.016, 0.002), range = c(1,5)) +
  scale_fill_gradientn("Harmonic centrality", colours = pal, breaks = seq(0.004, 0.016, 0.002), labels = seq(0.004, 0.016, 0.002)) +
  theme_graph() +
  theme(legend.position = "none")


# HARMONIC CENTRALITY PLOTS
# Colony 11 plots only

# TUBE NEST
# AGGRESSION ASSAY
TubeAggnHC11 <- Colony11HCDistFinal %>% 
  filter(Nest == "Tube" & Trial == "Aggn") %>%
  ggplot(aes(x = ScaledDist, y = ScaledHC)) +
  ggtitle("Colony 11 Tube Invader") +
  geom_point(key_glyph = large_points, size = 2.5, alpha = 0.75, color = "red") +
  geom_smooth(method = 'lm', se = FALSE, size = 1.5, color = "black") +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_text(size = 18, color = "white"),
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 20, color = "black", hjust = 0.1),
        legend.key = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  ylim(0.2, 0.9) + 
  xlim(0, 1)

# BASELINE ASSAY
TubePreHC11 <- Colony11HCDistFinal %>% 
  filter(Nest == "Tube" & Trial == "Pre") %>%
  ggplot(aes(x = ScaledDist, y = ScaledHC)) +
  ggtitle("Colony 11 Tube Baseline") +
  geom_point(key_glyph = large_points, size = 2.5, alpha = 0.75, color = "red") +
  geom_smooth(method = 'lm', se = FALSE, size = 1.5, color = "black") +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_text(size = 18, color = "white"),
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 20, color = "black", hjust = 0.1),
        legend.key = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  ylim(0.2, 0.9) + 
  xlim(0, 1)

# CIRCLE NEST
# AGGRESSION ASSAY
CircleAggnHC11 <- Colony11HCDistFinal %>% 
  filter(Nest == "Circle" & Trial == "Aggn") %>%
  ggplot(aes(x = ScaledDist, y = ScaledHC)) +
  ggtitle("Colony 11 Circle Invader")+
  geom_point(key_glyph = large_points, size = 2.5, alpha = 0.75, color = "blue") +
  geom_smooth(method = 'lm', se = FALSE, size = 1.5, color = "black") +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 20, color = "black", hjust = 0.1),
        legend.key = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  ylim(0.2, 0.9) + 
  xlim(0, 1)

# BASELINE ASSAY
CirclePreHC11 <- Colony11HCDistFinal %>% 
  filter(Nest == "Circle" & Trial == "Pre") %>%
  ggplot(aes(x = ScaledDist, y = ScaledHC)) +
  ggtitle("Colony 11 Circle Baseline") +
  geom_point(key_glyph = large_points, size = 2.5, alpha = 0.75, color = "blue") +
  geom_smooth(method = 'lm', se = FALSE, size = 1.5, color = "black") +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 20, color = "black", hjust = 0.1),
        legend.key = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  ylim(0.2, 0.9) + 
  xlim(0, 1)

# Compile the baseline harmonic centrality plots
TestHCDistPlot11.1 <- ggarrange(CirclePreHC11, TubePreHC11,
                          labels = c("(e)", "(f)"),
                          font.label = list(size = 20, face = "plain"),
                          ncol = 2, nrow = 1)

# Annotate the compiled plot
TestHCDistPlot11.1Annotate <- annotate_figure(TestHCDistPlot11.1,
                top = NULL,
                bottom = text_grob("Worker scaled dist entrance, frame 1", color = "black",
                                   size = 20, x = 0.53),
                left = text_grob("Harmonic centrality", color = "black",
                                 size = 20, rot = 90),
                right = NULL
)

# Compile the colony 11 baseline assay network plots
PreNetArrange <- ggarrange(Col11CirclePreNet, Col11TubePreNet, 
                          ncol = 2, nrow = 1)

# Compile the annotated baseline assay harmonic centrality plot and network plots
Col11_HC_Net_Pre <- ggarrange(TestHCDistPlot11.1Annotate, PreNetArrange,
          ncol = 1, nrow = 2)

# Save plot as a PDF
ggsave(file = "Fig3e_f.pdf", plot = Col11_HC_Net_Pre, width = 10.4, height = 10.4, units = "in")

# Compile the aggression harmonic centrality plots
TestHCDistPlot11.2 <- ggarrange(CircleAggnHC11, TubeAggnHC11,
                            labels = c("(g)", "(h)"),
                            font.label = list(size = 20, face = "plain"),
                            ncol = 2, nrow = 1)

# Annotate the compiled plot
TestHCDistPlot11.2Annotate <- annotate_figure(TestHCDistPlot11.2,
                                            top = NULL,
                                            bottom = text_grob("Worker scaled dist entrance, frame 1", color = "black",
                                                               size = 20, x = 0.53),
                                            left = text_grob("Harmonic centrality", color = "black",
                                                             size = 20, rot = 90),
                                            right = NULL
)

# Compile the colony 11 aggression assay network plots
AggnNetArrange <- ggarrange(Col11CircleAggnNet, Col11TubeAggnNet, 
                          ncol = 2, nrow = 1)

# Compile the annotated baseline assay harmonic centrality plot and network plots
Col11_HC_Net_Aggn <- ggarrange(TestHCDistPlot11.2Annotate, AggnNetArrange,
          ncol = 1, nrow = 2)

# Save plot as a PDF
ggsave(file = "Fig3g_h.pdf", plot = Col11_HC_Net_Aggn, width = 10.4, height = 10.4, units = "in")

