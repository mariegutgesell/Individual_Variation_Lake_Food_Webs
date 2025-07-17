##looking into lipid corrected vs. not lipid corrected results 

library(tidyverse)
library(readxl)


##Trying to recreate the dataframe and re-run analysis, trying to dig into results 


#read in BsM isotope data from Tim
iso_data <- read_excel("data/Copy of 2017March8 - Stable Isotope Data Analysis Integrated Master Worksheet - Bartley, T (1).xlsx", sheet = "Raw_SI_Data")


iso_data_sub <- iso_data %>%
  filter(!Lake_Name %in% c("Alexie", "Baptiste", "Chitty", "Drygeese", "Arc", "Blair", "Bluffy", "Bobs", "Canyon", "Jeanette", "Jubilee", "McCrea", "Nungesser", "Sowden", "Sup", "Wawang", "Wintering")) %>%##remove lakes with only baselines, no lake trout
  dplyr::select(c(1:16,46:63)) %>%
#  filter(Trophic_Group == "LakeTrout" | Trophic_Level == "Baseline" | Trophic_Category == "PFF" | Trophic_Category == "LFF") %>%
  rename(Included_in_Analysis = `include_in_analysis?`)

iso_data_sub <- iso_data_sub %>%
  filter(Source_Project_for_Data %in% c ("BSM_Project", "Algonquin_Lakes"))

iso_data_sub_lakes <- iso_data_sub %>%
  group_by(Lake_Name) %>%
  count()

str(iso_data_sub)

ggplot(iso_data_sub, aes(x = d13C, y = d15N, color = Trophic_Group)) +
  geom_point(size = 2) +
  theme_classic() +
  xlab("d13C") + 
  ylab("d15N") +
  facet_wrap(~Lake_Name)
##seeing if each lake has cisco, snails and mussels
bsm_sp_comp <- iso_data_sub %>%
  select(Lake_Name, Trophic_Group, d15N, d13C, d13C_Cor) %>%
  filter(Trophic_Group %in% c("LakeTrout", "Cisco", "Snail", "Mussel")) %>%
  group_by(Lake_Name, Trophic_Group) %>%
  count()

53*4

test <- bsm_sp_comp %>%
  group_by(Lake_Name) %>%
  count()
##20 of 53 lakes are missing a trophic group


##final, cleaned subset of data lakes from Tim/Tyler BSM data - 53 lakes
bsm_df <- iso_data_sub %>%
  select(Lake_Name, Habitat:Trophic_Group, Taxonomic_ID_Text:d13C_Cor, Source_Project_for_Data)


##Read in Tom Johnstons data 
tom_df <- read.csv("data/Tom_Johnson_data/tom_johnston_laketrout_baseline_isotope_data.csv")

tom_lakes <- tom_df %>%
  select(WBY) %>%
  unique()
##51 lakes 
ggplot(tom_df, aes(x = Del.13C, y = Del.15N, color = TAXON)) +
  geom_point(size = 2) +
  theme_classic() +
  xlab("d13C") + 
  ylab("d15N") +
  facet_wrap(~WBY)


##looking at sites with lake trout and baselines 
required_taxa <- c("LT", "CLAM", "SNAIL")

sites_with_all_three <- tom_df %>%
  distinct(WBY, TAXON) %>%     # remove duplicate taxa per site
  group_by(WBY) %>%
  summarize(has_all = all(required_taxa %in% TAXON)) %>%
  filter(has_all) %>%
  pull(WBY)

# Filter original dataframe to just those sites
tom_df_sub_1 <- tom_df %>%
  filter(WBY %in% sites_with_all_three)

test <- tom_df_sub_1 %>%
  select(WBY) %>%
  unique()
##19 sites that have both baselines 

57+19 

##then the 8 sites with snails but no mussels that can estimate baseline from using regression 
required_taxa_2 <- c("LT", "SNAIL")

sites_with_two <- tom_df %>%
  distinct(WBY, TAXON) %>%     # remove duplicate taxa per site
  group_by(WBY) %>%
  summarize(has_all = all(required_taxa_2 %in% TAXON)) %>%
  filter(has_all) %>%
  pull(WBY)

tom_df_sub_2 <- tom_df %>%
  filter(WBY %in% sites_with_two) %>%
  filter(!WBY %in% tom_df_sub_1$WBY)


tom_df_sub_3 <- rbind(tom_df_sub_1, tom_df_sub_2)


##cisco data for lakes within tom subset
tom_cisco_df <- read.csv("data/tom_johnston_cisco_isotope_data.csv") %>%
  filter(WBY %in% tom_df_sub_3$WBY)

tom_df_sub_4 <- rbind(tom_df_sub_3, tom_cisco_df)

tom_df_sub_all  <- tom_df_sub_4 %>%
  select(WBY, TAXON, Del.15N, Del.13C, C.N) %>%
  rename("Lake_Name" = WBY, "Taxonomic_ID_Text" = TAXON, "d15N" = Del.15N, "d13C" = Del.13C, "CN_Ratio" = C.N) %>%
  filter(Taxonomic_ID_Text %in% c("LT", "CLAM", "SNAIL", "CISCO")) %>%
  mutate(Habitat = case_when( 
    startsWith(Taxonomic_ID_Text, "LT") ~ "Coupler",
    startsWith(Taxonomic_ID_Text, "CLAM") ~ "Pelagic",
    startsWith(Taxonomic_ID_Text, "SNAIL") ~ "Littoral",
    startsWith(Taxonomic_ID_Text, "CISCO") ~ "Pelagic",
    )) %>%
  mutate(Trophic_Level = case_when( 
    startsWith(Taxonomic_ID_Text, "LT") ~ "Predator",
    startsWith(Taxonomic_ID_Text, "CLAM") ~ "Baseline",
    startsWith(Taxonomic_ID_Text, "SNAIL") ~ "Baseline",
    startsWith(Taxonomic_ID_Text, "CISCO") ~ "Forage Fish",
  )) %>%
  mutate(Trophic_Category = case_when( 
    startsWith(Taxonomic_ID_Text, "LT") ~ "PRED",
    startsWith(Taxonomic_ID_Text, "CLAM") ~ "PELAGIC",
    startsWith(Taxonomic_ID_Text, "SNAIL") ~ "LITTORAL",
    startsWith(Taxonomic_ID_Text, "CISCO") ~ "PFF",
  )) %>%
  mutate(Trophic_Group = case_when( 
    startsWith(Taxonomic_ID_Text, "LT") ~ "LakeTrout",
    startsWith(Taxonomic_ID_Text, "CLAM") ~ "Mussel",
    startsWith(Taxonomic_ID_Text, "SNAIL") ~ "Snail",
    startsWith(Taxonomic_ID_Text, "CISCO") ~ "Cisco",
  )) %>%
  mutate(d13C_Cor = d13C - 3.32 + 0.99 * CN_Ratio) %>%
   select(Lake_Name, Habitat, Trophic_Level, Trophic_Category, Trophic_Group, Taxonomic_ID_Text, d15N:d13C_Cor) %>%
  mutate(Source_Project_for_Data = "Tom_Johnston")


##under the assumption that only lakes with cisco sI data from tom has cisco, select only lakes with cisco 



test <- tom_df_sub_all %>%
  select(Lake_Name) %>%
  unique()
##27 lakes, sweet 

##list of species in each lake for toms data 
tom_sp_list <- read_excel("data/BsM_lakes_Tom_pred_prey_comm.xlsx")


filter_lakes_with_cisco <- function(df, lake_col = "Lake_Name", forage_col = "Co-habiting forage fishes") {
  lakes_with_cisco <- df %>%
    group_by(.data[[lake_col]]) %>%
    filter(any(grepl("\\bCISCO\\b", .data[[forage_col]], ignore.case = TRUE))) %>%
    ungroup()
  
  return(lakes_with_cisco)
}

tom_cisco_lakes <- filter_lakes_with_cisco(tom_sp_list)

tom_df_sub_cisco <- tom_df_sub_all %>%
  filter(Lake_Name %in% tom_cisco_lakes$Lake_Name)

test2 <- tom_df_sub_cisco %>%
  select(Lake_Name) %>%
  unique()

##22 lakes with cisco 

###Now for those 4 other lakes -- don't have the raw data as in non_lipid_bsm.R code, so using from the cleaned data 
si_4_sites <- read.csv("data/all_raw_iso_finalmay26.csv") %>%
  filter(Lake_Name %in% c("Muskoka", "Eagle", "Catchacoma", "Canoe_K")) %>%
  rename("Taxonomic_ID_Text" = Taxonomic_ID_Code) %>%
  mutate(Habitat = NA, Trophic_Level = NA) %>% ##feeling lazy to add these, but can 
  select(Lake_Name, Habitat, Trophic_Level, Trophic_Category, Trophic_Group, Taxonomic_ID_Text, d15N:d13C_Cor) %>%
  mutate(Source_Project_for_Data = "4 Extra Sites")


##okay join all raw isotope data together 
raw_si_df <- rbind(bsm_df, tom_df_sub_cisco) %>%
  rbind(si_4_sites)

test <- raw_si_df %>%
  select(Lake_Name) %>%
  unique()

##79 lakes with cisco 







##Conifer -- only LT and mussel, does have invert (fish fall within baselines), no snail
##Kay -- only Cisco and LT (has zoops and inverts)
##Abbess -- no mussel, but does have zooplankton 
##Argo -- no mussel, but does have zooplankton
##Beaverhouse -- no mussel or zoops - would need to estimate pelagic baseline
##Bell - no mussel, but does have zoops 
##Bending - no snail, no inverts - would need to estimate littoral baseline
##Canoe - no snails, does have inverts (crayfish, clam, and other non-described inverts)
##Carling - no mussels, but do have zoops 
##Cirrus - no snails, but have inverts (small, shrimplike critters)
##Crystal -- no mussel, but have zoops 
##Factor -- no mussels, but zooplankton 
##Indian -- no pelagic baselines, would need to estimate baselines 
##Kaiashkons -- no cisco (does look like it has other PFF, habitat says pelagic, and minnows but trophic category is LFF, so may be some coding mistake)
##Kakagi -- no cisco -- but looks like other pelagic forage fish? or at least says pelagic minnows 
##Lower Manitou -- no cisco but looks like other pelagic forage fish? or at least says pelagic minnows 
##Mold - no snail, but inverts (caddisfly, dragonfly, leech, insect)
##Pipestone - no mussel, but zooplankton
##Silver - no cisco, but other pff
##Wabaskang - no mussel, but has zoops 


##endikai - no mussel, would need to estimate pelagic baseline 
##george - no mussel, would need to estimate pelagic baseline
##johnnie - no mussel, would need to estimate pelagic baseline 
##long-1 - no mussel, would need to estimate pelagic baseline
##mesomikenda - no mussel, would need to estimate pelagic baseline
##scotia - no mussel, would need to estimate pelagic baseline
##trout - 2  - no mussel, would need to estimate pelagic baseline
##windy - no mussel, would need to estimate pelagic baseline 
##so of 84 lakes, 24 are missing some form of baseline
##of those 24, 10 we would need to estimate baselines using regressions (no other baselines in study)

##so of these, would only need to estimate baselines from 3 sites, others could use zooplankton and inverts 

##

##do all 84 lakes have cisco (or some pff) and lff? 




##USE Trophic_Group moving forward and Taxonomic_ID_text 



##if have other baseline from lake, eg., zooplankton or inverts, is that better to use than baseline regression estimates? look at how estimated baselines compare/behave relative to sampled baselines 

##Trying baseline regression as done by cassie to compare estimated baselines to real baselines of different taxon from same lake 

##How many lakes have cisco, or another PFF 



