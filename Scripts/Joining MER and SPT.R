#### Title
# PURPOSE: Joining MER and SPT
# AUTHOR: alerichardson | sch
# LICENSE: MIT
# DATE: 2022-10-11
# NOTES: 

#### Read In Data ============================================================================

mer_df = read_csv(here("Data", "MSD_mermaid_ave_cty.csv")) %>%
  select(OU = Country, `Tests Needed` = rtk_need)
algo_ft = read_csv(here("Data", "first_test_algo.csv"))

#### DATA WRANGLING ============================================================================

# Merge Item and Specify Other in oct.stock.proc and cut out Ukraine and PNG
oct.stock.proc2 = oct.stock.proc %>%
  mutate(Item = case_when(
    is.na(Specify_other) ~ Item,
    !is.na(Specify_other) ~ Specify_other
  )) %>%
  filter(OU != "Papua New Guinea" & OU != "Ukraine")

# Limit to the first test in the testing algorithm
oct.stock.first = tibble()
for(n in 1:length(algo_ft$Country)){
  temp = oct.stock.proc2 %>%
    filter(OU == algo_ft$Country[n])
  if(length(temp$OU)>0){
    temp = temp %>% 
      filter(Item == algo_ft$Item[n])
    oct.stock.first = oct.stock.first %>%
      bind_rows(temp)
  }
}

# Create a dataset with country, baseline, and the number of tests arriving per month, by committed or TBD
monthly.oct.stock = oct.stock.first %>%
  select(OU, Calendar_Year, Month, Procuring_Agency, Procured_Tests, Stock_on_Hand, Consumption_Rate, multiplier) %>%
  mutate(Stock_on_Hand = Stock_on_Hand*multiplier,
         Consumption_Rate = Consumption_Rate*multiplier,
         n = row_number()) %>%
  pivot_wider(id_cols = c("n", "OU", "Procuring_Agency", "Stock_on_Hand", "Consumption_Rate"), 
              names_from = c("Month", "Calendar_Year"), 
              values_from = Procured_Tests, 
              values_fill = 0) %>%
  select(OU, 
         Procuring_Agency, 
         Stock_on_Hand, 
         Consumption_Rate,
         October_2022,
         November_2022,
         December_2022,
         January_2023, 
         February_2023, 
         March_2023, 
         April_2023, 
         May_2023, 
         June_2023, 
         July_2023, 
         August_2023, 
         September_2023, 
         October_2023, 
         November_2023, 
         December_2023)

#### Tests Running Total ============================================================================
  
# Without TBD
monthly.planned = monthly.oct.stock %>%
  filter(Procuring_Agency != "TBD")

monthly.planned = monthly.planned %>%
  group_by(OU, Stock_on_Hand, Consumption_Rate) %>%
  summarize(October_2022 = sum(October_2022, na.rm = T),
            November_2022 = sum(November_2022, na.rm = T),
            December_2022 = sum(December_2022, na.rm = T),
            January_2023 = sum(January_2023, na.rm = T),
            February_2023 = sum(February_2023, na.rm = T),
            March_2023 = sum(March_2023, na.rm = T),
            April_2023 = sum(April_2023, na.rm = T),
            May_2023 = sum(May_2023, na.rm = T),
            June_2023 = sum(June_2023, na.rm = T),
            July_2023 = sum(July_2023, na.rm = T),
            August_2023 = sum(August_2023, na.rm = T),
            September_2023 = sum(September_2023, na.rm = T),
            October_2023 = sum(October_2023, na.rm = T),
            November_2023 = sum(November_2023, na.rm = T),
            December_2023 = sum(December_2023, na.rm = T)
            ) %>%
  mutate(October_2022 = October_2022+Stock_on_Hand) %>%
  select(-Stock_on_Hand)

for(n in 1:length(monthly.planned$OU)){
  for(m in 5:length(monthly.planned)){
    monthly.planned[n,m] <- monthly.planned[n,m] + monthly.planned[n,m-1] - monthly.planned$Consumption_Rate[n]
    if(monthly.planned[n,m]<0){
      monthly.planned[n,m]=0
    }
  }
}

monthly.planned = monthly.planned %>%
  ungroup() %>%
  select(-Stock_on_Hand) %>%
  mutate(TBD = "Not Included")

# With TBD

monthly.planned.TBD = monthly.oct.stock %>%
  group_by(OU, Stock_on_Hand, Consumption_Rate) %>%
  summarize(October_2022 = sum(October_2022, na.rm = T),
            November_2022 = sum(November_2022, na.rm = T),
            December_2022 = sum(December_2022, na.rm = T),
            January_2023 = sum(January_2023, na.rm = T),
            February_2023 = sum(February_2023, na.rm = T),
            March_2023 = sum(March_2023, na.rm = T),
            April_2023 = sum(April_2023, na.rm = T),
            May_2023 = sum(May_2023, na.rm = T),
            June_2023 = sum(June_2023, na.rm = T),
            July_2023 = sum(July_2023, na.rm = T),
            August_2023 = sum(August_2023, na.rm = T),
            September_2023 = sum(September_2023, na.rm = T),
            October_2023 = sum(October_2023, na.rm = T),
            November_2023 = sum(November_2023, na.rm = T),
            December_2023 = sum(December_2023, na.rm = T)
  ) %>%
  mutate(October_2022 = October_2022+Stock_on_Hand) %>%
  select(-Stock_on_Hand)

for(n in 1:length(monthly.planned.TBD$OU)){
  for(m in 5:length(monthly.planned.TBD)){
    monthly.planned.TBD[n,m] <- monthly.planned.TBD[n,m] + monthly.planned.TBD[n,(m-1)] - monthly.planned.TBD$Consumption_Rate[n]
    if(monthly.planned.TBD[n,m]<0){
      monthly.planned.TBD[n,m]=0
    }
  }
}

monthly.planned.TBD = monthly.planned.TBD %>%
  ungroup() %>%
  select(-Stock_on_Hand) %>%
  mutate(TBD = "Included")

monthly.planned = monthly.planned %>%
  bind_rows(monthly.planned.TBD)

#### Agency ============================================================================

oct.stock.agency = oct.stock.first %>% 
  select(OU, Procuring_Agency, Item, Stock_on_Hand, Procured_Tests) %>%
  group_by(OU, Procuring_Agency, Item) %>%
  summarize(Stock_on_Hand = mean(Stock_on_Hand, na.rm = T), Procured_Tests = sum(Procured_Tests, na.rm = T)) %>%
  pivot_wider(id_cols = c("OU", "Item", "Stock_on_Hand"), names_from = "Procuring_Agency", values_from = "Procured_Tests")

oct.stock.agency[is.na(oct.stock.agency)]<-0

oct.stock.agency = oct.stock.agency %>%
  left_join(mer_df) %>%
  rename(Country = OU,
         `First Test` = Item,
         `Stock on Hand` = Stock_on_Hand)

#### All Together ============================================================================

oct.stock.all = oct.stock.agency %>% 
  mutate(`Planned Tests` = `Global Fund`+`USAID/WCF`+`Country Government`+CDC+Other+`USAID (all other)`) %>%
  select(Country, `First Test`, `Stock on Hand`, `Planned Tests`, TBD, `Tests Needed`) %>%
  mutate(`Sufficient Tests` = case_when(
    (`Stock on Hand`+`Planned Tests`)>=`Tests Needed` ~ "Yes",
    (`Stock on Hand`+`Planned Tests`)<`Tests Needed` ~ "No"
  ))
  
#### Writing to Excel ============================================================================

write_csv(oct.stock.agency, here("Data", "Agency MER-SPT Crosswalk.csv"))
write_csv(oct.stock.all, here("Data", "Total MER-SPT Crosswalk.csv"))

#### VIZ ============================================================================

oct.stock.onhand = oct.stock.first %>% 
  select(OU, Procured_Tests = Stock_on_Hand) %>%
  filter(!duplicated(OU)) %>%
  mutate(labels = "Stock on Hand")

oct.stock.total = oct.stock.first %>% 
  bind_rows(oct.stock.onhand) %>%
  select(OU, labels = Procuring_Agency, Procured_Tests) %>%
  group_by(OU) %>%
  summarize(Procured_Tests = sum(Procured_Tests, na.rm = T)) %>%
  mutate(labels = "All Tests",
         parent = "") %>%
  rename(Country = OU)
  

oct.stock.sunburst = oct.stock.first %>% 
  select(OU, labels = Procuring_Agency, Procured_Tests) %>%
  group_by(OU, labels) %>%
  summarize(Procured_Tests = sum(Procured_Tests, na.rm = T)) %>%
  bind_rows(oct.stock.onhand) %>%
  rename(Country = OU) %>%
  bind_rows(oct.stock.total) %>%
  mutate(parent = case_when(
    labels %in% c("TBD", "Country Government") ~ "Unreliable",
    labels == "All Tests" ~ "",
    TRUE ~ "Reliable"
  ))

oct.stock.reliable = oct.stock.sunburst %>%
  filter(labels != "All Tests") %>%
  mutate(reliable = case_when(
    labels %in% c("TBD", "Country Government") ~ "Unreliable",
    TRUE ~ "Reliable"
  )) %>%
  group_by(Country, reliable) %>%
  summarize(Procured_Tests = sum(Procured_Tests, na.rm = T)) %>%
  rename(labels = reliable) %>%
  mutate(parent = "All Tests")

# oct.stock.remaining = oct.stock.mer %>%
#   rename(MER_target = Procured_Tests) %>%
#   select(-labels) %>%
#   right_join(oct.stock.reliable) %>%
#   mutate(difference = Procured_Tests-MER_target) %>%
#   select(Country, MER_target, Procured_Tests, labels, parent, difference) %>%
#   mutate(labels = case_when(
#     labels == "Unreliable" ~ "",
#     labels == "Reliable" & difference>1 ~ "Potential Surplus",
#     labels == "Reliable" & difference<1 ~ "Potential Deficit"
#   )) %>%
#   select(Country, MER_target, labels) %>%
#   filter(labels !="")
#   
# oct.stock.remaining2 = oct.stock.reliable %>%
#   group_by(Country) %>%
#   summarize(Procured_Tests = sum(Procured_Tests, na.rm = T)) %>%
#   left_join(oct.stock.remaining) %>%
#   mutate(Procured_Tests = Procured_Tests-MER_target,
#          parent = "") 
  
oct.stock.sunburst = oct.stock.sunburst %>%
  bind_rows(oct.stock.reliable)



#### SPINDOWN ============================================================================
