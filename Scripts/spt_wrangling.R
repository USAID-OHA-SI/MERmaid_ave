#### Title
# PURPOSE: Cleaning and Analysis of SPT for MER
# AUTHOR: alerichardson | sch
# LICENSE: MIT
# DATE: 2022-10-07
# NOTES: 


#### LOAD DATA ============================================================================  

procure.data = read_csv(here("Data", "global_procure_data22.csv"))

stock.data = read_csv(here("Data", "global_stock_data22.csv"))

#### DATA WRANGLING ============================================================================
  
# Replicating tab "Oct 22 RTK SOH"
oct.stock = stock.data %>%
  filter(Fiscal_Year == 2023 & 
           Month == "October" &
           Minor_Category == "HIV Tests")

# Replicating tab "COP22 RTK Procs"
cop22.proc = procure.data %>%
  filter(Funded_by_Year == "COP22",
         Minor_Category == "HIV Tests") %>%
  select(OU,
         Month,
         Procuring_Agency,
         Status,
         Minor_Category,
         Item,
         Procured_Amount,
         Specify_other) %>%
  mutate(multiplier = case_when(
    is.na(Specify_other) ~ str_extract_all(Item, '(?<=, )\\d{1,}'),
    !is.na(Specify_other) ~ str_extract_all(Specify_other, '(?<=, )\\d{1,}')
  ))

cop22.proc$multiplier[is.na(cop22.proc$multiplier==0)]<-NA

# Combining for procured vs. SOH as of October
oct.stock.join = oct.stock %>%
  select(OU, 
         Item, 
         Procured_Amount,
         Consumption_Rate,
         Initial_Stock,
         Stock_on_Hand,
         Months_of_Stock,
         Projected_Shortfall)

oct.stock.proc = cop22.proc %>%
  left_join(oct.stock.join, by = c("OU" = "OU",
                                   "Item" = "Item"))
