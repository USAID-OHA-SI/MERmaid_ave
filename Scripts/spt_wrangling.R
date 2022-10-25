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
           Minor_Category == "HIV Tests") %>%
  mutate(Item = str_remove_all(Item, "\\\x99"))

# Replicating tab "COP22 RTK Procs"
cop22.proc = procure.data %>%
  filter(Funded_by_Year == "COP22",
         Minor_Category == "HIV Tests") %>%
  select(OU,
         Calendar_Year,
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
  )) %>%
  mutate(Item = str_remove_all(Item, "\\\x99")) %>%
  mutate(Specify_other = str_remove_all(Specify_other, "\\\x99"))

cop22.proc$multiplier[is.na(cop22.proc$multiplier==0)]<-NA

# Combining for procured vs. SOH as of October
oct.stock.join = oct.stock %>%
  select(OU, 
         Item, 
         Initial_Stock,
         Stock_on_Hand,
         Months_of_Stock,
         Projected_Shortfall,
         Consumption_Rate)

oct.stock.proc = cop22.proc %>%
  left_join(oct.stock.join, by = c("OU" = "OU",
                                   "Item" = "Item"))

no.mult = read_csv(here("Data", "NoMult.csv")) %>%
  mutate(Item = str_remove_all(Item, "™"))

oct.stock.proc = oct.stock.proc %>%
  left_join(no.mult) 

oct.stock.proc$`Multiplier used?`[oct.stock.proc$`Multiplier used?` == "N"]<-FALSE
oct.stock.proc$`Multiplier used?`[is.na(oct.stock.proc$`Multiplier used?`)]<-TRUE

na.mult = oct.stock.proc %>%
  filter(is.na(multiplier)) %>%
  group_by(Specify_other, multiplier) %>%
  summarize()

#write_csv(na.mult, here("Data", "na.mult.csv"))

na.mult = read_csv(here("Data", "na.mult.csv"))

oct.stock.proc = oct.stock.proc %>%
  mutate(multiplier = unlist(multiplier)) %>%
  left_join(na.mult, by = c("Specify_other"="Specify_other")) %>%
  mutate(multiplier = case_when(
    is.na(multiplier.x) ~ multiplier.y,
    is.na(multiplier.y) ~ as.numeric(multiplier.x)
  )) %>% 
  select(-multiplier.x, -multiplier.y) %>%
  mutate(Procured_Tests = case_when(
    `Multiplier used?` == TRUE ~ Procured_Amount*multiplier,
    `Multiplier used?` == FALSE ~ Procured_Amount
  ))
