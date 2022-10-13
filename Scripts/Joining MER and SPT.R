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

#### DATA WRANGLING - First Test ============================================================================
oct.stock.proc2 = oct.stock.proc %>%
  mutate(Item = case_when(
    is.na(Specify_other) ~ Item,
    !is.na(Specify_other) ~ Specify_other
  )) %>%
  filter(OU != "Papua New Guinea" & OU != "Ukraine")

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

#### DATA WRANGLING - Agency ============================================================================

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

#### DATA WRANGLING - All ============================================================================

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
