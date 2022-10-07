# PURPOSE: Munge and Analysis of supply chain and MER data
# AUTHOR: jdavis | sch
# LICENSE: MIT
# DATE: 2022-10-06
# NOTES: This script will read in and munge the OU/IM MSD for use with the SPT data
# per requirements: pulling tot_num FY23 targets for HTS_TST, PREP_CT, PREP_NEW

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
      summarise(targets = sum(targets, na.rm = T))
  
  df1 %>% write_csv(file.path(dataout, "MSD_mermaid_ave.csv"))
  
  drive_upload(media = "dataout/MSD_mermaid_ave.csv",
               path = as_id("1jWIHhL3m7omtrkhSw_Q6iEPx8-Oxl27d"),
               overwrite = T)
      

      
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================
