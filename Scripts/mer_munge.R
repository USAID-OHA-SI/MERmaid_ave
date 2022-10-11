# PURPOSE: Munge and Analysis of supply chain and MER data
# AUTHOR: jdavis | sch
# LICENSE: MIT
# DATE: 2022-10-06
# NOTES: This script will read in and munge the OU/IM MSD for use with the SPT data
# per requirements: pulling tot_num FY23 targets for HTS_TST, PREP_CT, PREP_NEW
# add: rtks_needed

# LOCALS & SETUP ============================================================================

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
glamr::load_secrets()



# LOAD DATA ============================================================================  

  #
  df <- gophr::read_msd(file.path(merdata, "MER_Structured_Datasets_OU_IM_FY20-23_20220916_v2_1.zip"))

# MUNGE ============================================================================
  
  #munge per requirements
    
  df1 <- df %>%
      filter(indicator %in% c("HTS_TST", "PrEP_CT", "PrEP_NEW"),
             standardizeddisaggregate == "Total Numerator",
             fiscal_year == 2023) %>% 
      reshape_msd("semi-wide") %>% 
      group_by(operatingunit, period, indicator) %>% 
      summarise(targets = sum(targets, na.rm = T)) %>%
      mutate(rtk_need = case_when(indicator %in% c("HTS_TST","PrEP_NEW") ~ targets,
                              indicator == "PrEP_CT" ~ (targets*2)))
  
  #save locally
  df1 %>% write_csv(file.path(dataout, "MSD_mermaid_ave.csv"))
  
  #write to drive
  drive_upload(media = "dataout/MSD_mermaid_ave.csv",
               path = as_id("1jWIHhL3m7omtrkhSw_Q6iEPx8-Oxl27d"),
               overwrite = T)
      

      
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================
