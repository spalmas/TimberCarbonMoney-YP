#startup file for RIL-Simulation
#run this before trying code. this is not useful for the app

rm(list=ls()) #will remove ALL objects 

library(grid)
library(gridExtra)
library(mgcv) #Only for in.out function importFrom(mgdv, in.out) in skidding.mortality.R 
library(shiny)
library(tidyverse)
library(truncnorm)

diameter.eqs <- read_csv('data/RIL-tab-TNC-growth.csv')
regen.params <- read_csv('data/regenation_params.csv')
species <- read_csv('data/RIL-tab-species.csv')
INFyS2009 <-   read_csv('data/ArboladoQRSelected.csv') #List of trees in 2009 INFyS from the selected species

source('R/forest_randomizerINFyS.R')
source('R/get_agb.R')
source('R/get_diameter_growth.R')
source('R/get_enrichment.R')
source('R/get_harvest.R')
source('R/get_height.R')
source('R/get_price.R')
source('R/get_volume.R')
source('R/get_harvest.R')
source('R/get_MgC.R')
source('R/get_mortality.R')
source('R/get_mortality_DF.R')
source('R/get_mortality_skidding.R')
source('R/get_regeneration.R')
source('R/ggplot_params.R')
source('R/mean_sd.R')
source('R/multiplot.R')

source('R/simulator.R')
