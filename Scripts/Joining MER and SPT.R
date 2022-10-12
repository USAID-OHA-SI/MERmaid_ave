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

  #  

#### SPINDOWN ============================================================================
