# AntColonyPerformance compendium

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/Gchism94/AntColonyPerformance/main?urlpath=rstudio)
[![.github/workflows/docker-hub.yml](https://github.com/Gchism94/AntColonyPerformance/actions/workflows/docker-hub.yml/badge.svg)](https://github.com/Gchism94/AntColonyPerformance/actions/workflows/docker-hub.yml)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<a href="https://de.cyverse.org/apps/de/48b6e7ae-8b64-11ec-92dc-008cfa5ae621/launch" target="_blank"><img src="https://img.shields.io/badge/Verse-latest-blue?style=plastic&logo=rstudio"></a>
[![DOI](https://zenodo.org/badge/498113063.svg)](https://zenodo.org/badge/latestdoi/498113063)

## A compendium of code, data, and author's manuscript draft for In Preparation work

## This pre-release can be cited as the following: 
> Greg T. Chism. (2022). Gchism94/AntColonyPerformance: (Pre-release) Research Compendium for In preparation work (v0.1.0). Zenodo. https://doi.org/10.5281/zenodo.6872019

## Overview
This repository is organized as a reproducible research compendium. 
Click the [![Binder](http://mybinder.org/badge.svg)](http://beta.mybinder.org/v2/gh/Gchism94/AntColonyPerformance/main?urlpath=rstudio) button above to explore in an interactive RStudio session.  Binder uses [rocker-project.org](https://rocker-project.org) Docker images to ensure a consistent and reproducible computational environment.  These Docker images can also be used locally. 

This repository promotes the use of [CyVerse](https://cyverse.org/), a cloud-based alternative to the RStudio IDE, and a more scalable option to RStudio Cloud. <a href="https://de.cyverse.org/apps/de/48b6e7ae-8b64-11ec-92dc-008cfa5ae621/launch" target="_blank"></a>

To use CyVerse, subscribe at the top-right corner found in the link here. Next click the badge above to open the latest version of rocker-verse, which opens an RStudio container with the tidyverse meta-package installed.

Once loaded, run git clone https://github.com/Gchism94/Data7_EDA_In_R_Workshops.git to clone this repository and run everything as normal.

## File Organization

    analysis/
    |
    ├── paper/
    │   ├── paper.Rmd       # this is the main document to edit
    │   └── paper.pdf       # this is an elsevier .pdf written from paper.Rmd
    |
    ├── figures/            # location of the figures produced by the scripts in R
    |
    ├── data/
    │   ├── RawData/        # data obtained from elsewhere
    │   └── RefData/        # data used to obtain final data and during the analysis
    |   
    ├── supplementary-materials/
    │   ├── Supplementary_Figures/     
    |   |                   # supplementary figures for the main manuscript
    │   └── Supplementary_Tables/      
    |                       # supplementary tables for the main manuscript 
    |
    └── R                   # Run in the following order (also see associated README.md
        ├── Stat_boxplot_custom.R
        |                   # Custom box plot function to extend the whiskers to full data range (0th and 100th percentiles)
        ├── AggnPerformance.R        
        |                   # R script used to determine colony performance against an invader, including code for analyses and figures
        ├── ABCTracker_Movement.R 
        |                   # R script used to transform raw ABCTracker movement data to analyze colony activity
        ├── MovementBins_Density.R  
        |                   # R script used to bin data from ABCTracker_Movement.R to calculate worker traffic jams in relation to worker density
        ├── ABCTracker_AdjacencyMatrix.R  
        |                   # R script used to create adjacency matrices from raw ABCTracker movement data
        └── ABCTracker_Networks.R  
                            # R script used to create and analyze worker interaction networks from adjacency matrices per ABCTracker_AdjacencyMatrix.R
        

An `Rmd` notebook and associated pdf for the manuscript can be found in [analysis](/paper). This notebook produces a .pdf document in elsevier format.

README.md files are included in all subdirectories with explanations or contents related to the paper. It should also provide a useful starting point for extending and exploring these materials for other projects.

Or to explore the code locally, clone or download this repository into RStudio or your preferred environment and install the compendium by running `devtools::install()`.  To install additional dependencies used only in formatting the figures, use `devtools::install(dep=TRUE)`.  

