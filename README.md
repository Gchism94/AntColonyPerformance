# README for Nest shape does not affect ant colony performance against a nest invader despite altered worker movement and communication

***

## Overview
Data and R script used for the manuscript: Nest shape does not affect ant colony performance against a nest invader despite altered worker movement and communication

***

## Purpose of the study
### Investigating how nest shape influences how _Temnothorax rugatulus_ colonies perform essential tasks, specifically nest defense. This includes colony performance against a conspecific nest invader, communication through worker interaction networks, and movement and traffic jams through the nest. 

## Dependencies 
##### Scripts for this manuscript should be executed in the following order: 
1. Stat_boxplot_custom.R - Custom boxplot function, replacing geom_boxplot() in ggplot2. The function extends the whisker range to the data range
2. AggnPerformance.r - Functions, plots, and analyses for colony performance in removing a conspecific nest invader
3. ABCTracker_Networks.R - Functions, plots, and analyses for worker interaction networks
4. ABCTracker_Movement.R - Functions, plots, and analyses for worker traffic analysis (excluding density)
5. MovementBins_Density.R - Functions, plots, and analyses for worker traffic analysis in nest sections (density in the nest)

##### TO REPRODUCE ADJACENCY MATRICES
* ABCTracker_AdjacencyMatrix.R

##### Several packages are required, however all are loaded through the package "pacman", so be certain to install this package before running any other code.
##### See the following documentation for further information on the "pacman" package: https://www.rdocumentation.org/packages/pacman/versions/0.5.1 

***

## Structure of the data
### COLONY PERFORMANCE 
#### Aggression_Data_Working.CSV
###### Raw experimental data regarding invader removal 
* Colony: Unique experimental colony identifiers
* Nest: The nest shape treatment (Tube / Circle)
* Trial: The experimental trial (Pre / Aggn), Pre = baseline and Aggn = Invader in the manuscript
* Nest.Numb: The nest number in relation to whether the occupied nest was the first or second for that colony (1, 2) 
* Assay: The invader assay number for the nest & trial combination (1-5)
* Inv.Insert: Time (secs) in the assay video that the invader was inserted into the nest
* Inv.Remov: Time (secs) in the assay video that the invader was removed from the nest (max = 900secs, indicating the invader was not removed)
* Density: The density treatment (High / Low) 
* Attacking.Max: The maximum number of workers attacking the invader in the nest
* Attack.Remove: The number of workers attacking the invader when it was removed
* Distance: The raw farthest distance from the entrance the invader penetrated the nest 
* MaxDist: Max possible shortest distance from the nest entrance
* ScaledDist: Quotient from Distance / MaxDist

***

###  INTERACTION NETWORKS
#### All matrix .csv's, e.g., Colony5TubeAggnRMatrix.csv
##### Adjacency matrices that show the number of reciprocal and non-reciprocal interactions between two workers
* Columns and rows are symmetrically filled with all worker IDs from ABCTracker
* Cells are the total number of times an interaction occurred between a pair of individuals throughout the assay video

***

### WORKER TRAFFIC
#### All raw ABCTracker outputs, e.g., Colony5_2CircleOct_18AggnTest (names reflect original ABCTracker process names) 
##### Raw ABCTracker outputs used for all traffic analyses and to produce interaction adjacency matrices (see above) 
* id: ABCTracker assigned unique tracklet identifier (representing an individual ant)
* frames: The frame number from the input video
* locX: x-coordinate location (px) of the tracked ant 
* locY: y-coordinate location (px) of the tracked ant
* orientation: Direction the tracked ant is facing (0-360 degrees)
* size1Px: Width of the tracklet box assigned to an individual ant (px)
* size2Px: Length of the tracklet box assigned to an individual ant (px)
* speedPxPerSec: Speed of the tracked ant (px/s)
* Interpolated: IGNORE
* headLocX: x-coordinate location (px) of the head of the tracked ant
* headLocY: y-coordinate location (px) of the head of the tracked ant

***

### REFERENCE DATA (ABCTracker_Movement.R)
#### Reference coordinates for the entrance of nest sections front-to-back and shortest distance to the entrance  
##### AggnStudyTubeRefCoords.csv (Tube nest); AggnStudyCircleRefCoords.csv (Circle nest)
* Colony: Unique experimental colony identifiers
* Nest: The nest shape treatment (Tube / Circle)
* Trial: The experimental trial (Pre / Aggn), Pre = baseline and Aggn = Invader in the manuscript
* Coord: The reference coordinate number
* XREF: x-coordinate reference
* YREF: y-coordinate reference 

***

### REFERENCE DATA (MovementBins_Density.R)
#### Reference binning coordinates to group ABCTracker output coordinates into nest sections
* Colony: Unique experimental colony identifiers
* Nest: The nest shape treatment (Tube / Circle)
* Trial: The experimental trial (Pre / Aggn), Pre = baseline and Aggn = Invader in the manuscript
* Coord: The reference coordinate number
* X: x-coordinate reference
* Y: y-coordinate reference 

***
