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
require(plotly)
require(xlsx)
require(readxl)


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
joining = here("Scripts", "Joining MER and SPT.R")
report = here("Scripts", "MERmaid_reports.Rmd")

#### Run ============================================================================

source(mer_munge)
source(spt_wrangling)
source(joining)

for(n in unique(oct.stock.first$OU)){
  country = n
  fy = 2023
  filename = paste0("MERmaid_report_",country,".html")
  rmarkdown::render(report, output_file = here("Dataout", filename))
}


