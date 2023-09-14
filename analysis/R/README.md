# README for Nest shape does not affect ant colony performance against a nest invader despite altered worker movement and communication

***

## Overview
Data and R script used for the manuscript: Nest shape does not affect ant colony performance against a nest invader despite altered worker movement and communication

***

## Purpose of the study
### Investigating how nest shape influences how _Temnothorax rugatulus_ colonies perform essential tasks, specifically nest defense. This includes colony performance against a conspecific nest invader, communication through worker interaction networks, and movement and traffic jams through the nest. 

## Dependencies 
##### Scripts for this manuscript should be executed in the following order (but it is not required): 
1. AggnPerformance.r - Functions, plots, and analyses for colony performance in removing a conspecific nest invader
2. ABCTracker_Networks.R - Functions, plots, and analyses for worker interaction networks
3. ABCTracker_Movement.R - Functions, plots, and analyses for worker traffic analysis (excluding density)
4. MovementBins_Density.R - Functions, plots, and analyses for worker traffic analysis in nest sections (density in the nest)

##### TO REPRODUCE ADJACENCY MATRICES
* ABCTracker_AdjacencyMatrix.R

##### Several packages are required, however all are loaded through the package "pacman", so be certain to install this package before running any other code.
##### See the following documentation for further information on the "pacman" package: https://www.rdocumentation.org/packages/pacman/versions/0.5.1 

***
