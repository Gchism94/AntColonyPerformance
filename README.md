# README for Nest shape does not affect ant colony performance against a nest invader despite altered worker movement and communication

***

## Overview
Data and R script used for the manuscript: Nest shape does not affect ant colony performance against a nest invader despite altered worker movement and communication

***

## Purpose of the study
### Investigating how nest shape influences how _Temnothorax rugatulus_ colonies perform essential tasks, specifically nest defense. This includes colony performance against a conspecific nest invader, communication through worker interaction networks, and movement and traffic jams through the nest. 

## Dependencies 
##### Scripts for this manuscript should be executed in the following order: 
1. Stat_boxplot_custom.R
2. AggnPerformance.r
3. ABCTracker_Networks.R
4. ABCTracker_Movement.R
5. MovementBins_Density.R

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

###  INTERACTION NETWORKS
#### All matrix .csv's, e.g., Colony5TubeAggnRMatrix.csv
##### Adjacency matrices that show the number of reciprocal and non-reciprocal interactions between two workers
* Columns and rows are symmetrically filled with all worker IDs from ABCTracker
* Cells are the total number of times an interaction occurred between a pair of individuals throughout the assay video
