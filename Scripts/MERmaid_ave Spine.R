#### Title
# PURPOSE: MERmaid_ave Spine
# AUTHOR: alerichardson | sch
# LICENSE: MIT
# DATE: 2022-10-07
# NOTES: 

#### LOCALS & SETUP ============================================================================


# Libraries
library(glitr)
library(glamr)
library(gisr)
library(Wavelength)
library(gophr)
library(tidyverse)
library(scales)
library(sf)
library(extrafont)
library(tidytext)
library(patchwork)
library(ggtext)
library(here)
library(googledrive)



# Set paths  
data   <- "Data"
dataout <- "Dataout"
images  <- "Images"
graphs  <- "Graphics"

merdata <- glamr::si_path("path_msd")
rasdata <- glamr::si_path("path_raster")
shpdata <- glamr::si_path("path_vector")
datim   <- glamr::si_path("path_datim")  


# Functions  
load_secrets()

#### Script Paths ============================================================================  

mer_munge = here("Scripts", "mer_munge.R")
spt_wrangling = here("Scripts", "spt_wrangling.R")

#### Run ============================================================================

source(mer_munge)
source(spt_wrangling)