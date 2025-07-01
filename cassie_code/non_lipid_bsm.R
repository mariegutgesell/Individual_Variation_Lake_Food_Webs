##### redoing analysis with non lipid corrected data
# May 8/ 2025


#set wd
setwd("~/Desktop/BSM")

#load packages
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(car)

library(readxl)


###DATASET FORMATION#####

#read in BsM isotope data from Tim
iso_data <- read_excel("data/Copy of 2017March8 - Stable Isotope Data Analysis Integrated Master Worksheet - Bartley, T (1).xlsx", sheet = "Raw_SI_Data")


iso_data_sub <- iso_data %>%
  filter(!Lake_Name %in% c("Alexie", "Baptiste", "Chitty", "Drygeese", "Arc", "Blair", "Bluffy", "Bobs", "Canyon", "Jeanette", "Jubilee", "McCrea", "Nungesser", "Sowden", "Sup", "Wawang", "Wintering")) %>%##remove lakes with only baselines, no lake trout
  dplyr::select(c(1:16,46:63)) %>%
  filter(Trophic_Group == "LakeTrout" | Trophic_Level == "Baseline" | Trophic_Category == "PFF") %>%
  rename(Included_in_Analysis = `include_in_analysis?`)

iso_data_sub <- iso_data_sub %>%
  filter(Source_Project_for_Data %in% c ("BSM_Project", "Algonquin_Lakes"))

iso_data_sub_lakes <- iso_data_sub %>%
  group_by(Lake_Name) %>%
  count()

##53 lakes total - this matches the lake attributes data set from dataset_formation_April5.R




#Now load in other data - muskoka lake, eagle lake, catchacoma lake and canoe kingston 

library(readxl)

#eagle lake
eagle_lt <- read_excel("Data/Eagle_ISData.xlsx", sheet = "Eag_LT")
eagle_bl <- read_excel("Data/Eagle_ISData.xlsx", sheet = "Eag_Sources")

eagle_lt <- eagle_lt %>%
  filter(Lake_Name == "Eagle")

eagle_lt <- eagle_lt %>%
  rename(d15N = '15N',
         d13C = '13C',
         d13C_corr = '13CCor')

#take out columns that don't match up with baseline df
eagle_lt <- eagle_lt %>%
  dplyr::select(-RWT, -FLEN, -TLEN)

eagle_bl <- eagle_bl %>%
  dplyr::select(Lake_Name, SI_ID, Type, Species, '15N', '13C', CN, '13 c corr')

eagle_bl <- eagle_bl %>%
  rename(d15N = '15N',
         d13C = '13C',
         d13C_corr = '13 c corr')

#now merge the two using rbind
eagle_iso <- rbind(eagle_bl, eagle_lt) 
#this is our eagle lake DF for tp and lc calculations

#biplot to check distribution
eagle_plot <- subset %>%
  ggplot(aes(x = d13C, y = d15N, color = Species)) +
  geom_point(size = 2) +
  theme_classic() +
  xlab("d13C") + 
  ylab("d15N") 

eagle_plot

#biplot with snails mussels and lake trout
subset<-eagle_iso %>%
  filter(Species %in% c("81", "snail", "mussel"))


#canoe kingston  - canoe_k
canoe_lt <- read_excel("Data/Canoe_ISDataL.xlsx", sheet = "CAN_LT")
canoe_bl <- read_excel("Data/Canoe_ISDataL.xlsx", sheet = "CAN_Sources")

canoe_lt <- canoe_lt %>%
  rename(Lake_Name = 'Lake Name')

canoe_lt <- canoe_lt %>%
  filter(Lake_Name == "Canoe")

canoe_lt <- canoe_lt %>%
  rename(d15N = '15N',
         d13C = '13C',
         d13C_corr = '13Ccorr')

canoe_lt <- canoe_lt %>%
  dplyr::select(-RWT, -FLEN, -TLEN)

canoe_bl <- canoe_bl %>%
  dplyr::select(Lake_Name, SI_ID, Type, Species, '15N...5', '13C...6', CN, '13Ccorr')

#rename to canoe_k so we dont get confused with the algonquin canoe lake
canoe_lt <- canoe_lt %>%
  mutate(Lake_Name = "Canoe_K")

canoe_bl <- canoe_bl %>%
  rename(d15N = '15N...5',
         d13C = '13C...6',
         d13C_corr = '13Ccorr')

canoe_bl <- canoe_bl %>%
  mutate(Lake_Name = "Canoe_K")

#now merge the two using rbind
canoe_iso <- rbind(canoe_bl, canoe_lt) 

#this is our canoe_k lake DF for tp and lc calculations

#biplot to check distribution
canoe_plot <- canoe_iso %>%
  ggplot(aes(x = d13C, y = d15N, color = Species)) +
  geom_point(size = 2) +
  theme_classic() +
  xlab("d13C") + 
  ylab("d15N") 

canoe_plot


#Catchacoma lake
catch_lt <- read_excel("Data/Catchacoma_ISData.xlsx", sheet = "Catch_LT")
catch_bl <- read_excel("Data/Catchacoma_ISData.xlsx", sheet = "Catch_Sources")

catch_lt <- catch_lt %>%
  rename(Lake_Name = 'Lake Name')

catch_lt <- catch_lt %>%
  filter(Lake_Name == "Catchacoma")

catch_lt <- catch_lt %>%
  rename(d15N = '15N',
         d13C = '13C',
         d13C_corr = '13Cc')

catch_lt <- catch_lt %>%
  dplyr::select(-RWT, -FLEN, -TLEN)

catch_bl <- catch_bl %>%
  rename(Lake_Name = '...1')

catch_bl <- catch_bl %>%
  dplyr::select(Lake_Name, SI_ID, Type, Species, '15N...5', '13C...6', CN, '13C corr...8')

catch_bl <- catch_bl %>%
  rename(d15N = '15N...5',
         d13C = '13C...6',
         d13C_corr = '13C corr...8')

#now merge the two using rbind
catch_iso <- rbind(catch_bl, catch_lt) 

str(catch_iso)
catch_iso$d15N <- as.numeric(catch_iso$d15N)
catch_iso$d13C_corr <- as.numeric(catch_iso$d13C_corr)
#this is our catch lake DF for tp and lc calculations

#biplot to check distribution
catch_plot <- subset %>%
  ggplot(aes(x = d13C, y = d15N, color = Species)) +
  geom_point(size = 2) +
  theme_classic() +
  xlab("d13C") + 
  ylab("d15N") 

catch_plot

##subset
subset<-catch_iso %>%
  filter(Species %in% c("81", "snail", "clam"))

catch_plot <- subset %>%
  ggplot(aes(x = d13C, y = d15N, color = Species)) +
  geom_point(size = 2) +
  theme_classic() +
  xlab("d13C") + 
  ylab("d15N") +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),  # Adjust the x-axis label size
    axis.title.y = element_text(size = 16)   # Adjust the y-axis label size
  )




##rename clam to mussel to match the other lake dfs
catch_iso <- catch_iso %>%
  mutate(Species = ifelse(Species == "clam", "mussel", Species))


#muskoka lake
muskoka_lt <- read_excel("Data/Muskoka_ISDataC.xlsx", sheet = "Muskoka_LT")
muskoka_bl <- read_excel("Data/Muskoka_ISDataC.xlsx", sheet = "Muskoka_Sources")


muskoka_lt <- muskoka_lt %>%
  filter(Lake_Name == "Muskoka L.")

muskoka_lt <- muskoka_lt %>%
  mutate(Lake_Name = "Muskoka")

muskoka_bl <- muskoka_bl %>%
  rename(Lake_Name = 'Lake Name')

muskoka_bl <- muskoka_bl %>%
  filter(Lake_Name == "Muskoka")

muskoka_lt <- muskoka_lt %>%
  rename(d15N = '15N',
         d13C = '13C',
         d13C_corr = '13Corr')

muskoka_lt <- muskoka_lt %>%
  dplyr::select(-RWT, -FLEN, -TLEN)

muskoka_bl <- muskoka_bl %>%
  dplyr::select(Lake_Name, SI_ID, Type, Species, '15N...5', '13C...6', CN, '13C corr...8')

muskoka_bl <- muskoka_bl %>%
  rename(d15N = '15N...4',
         d13C = '13C...5',
         d13C_corr = '13c corr...7')

muskoka_bl <- muskoka_bl %>%
  dplyr::select(Lake_Name, Type, Species, 'd15N', 'd13C', CN, 'd13C_corr')

#muskoka baselines do not have a SI_ID - we will add this column and randomly assign SI_ID to these baselines
#make it MA1 - MA26

muskoka_bl <- muskoka_bl %>%
  mutate(SI_ID = paste0("MA", 1:26))


#now merge the two using rbind
muskoka_iso <- rbind(muskoka_bl, muskoka_lt) 

#merge all of these into one df
iso0 <- rbind(muskoka_iso, catch_iso) 
iso1 <- rbind(iso0, canoe_iso)
iso2 <- rbind(iso1, eagle_iso)

##this df we will merge with our iso_data_sub dataframe
#first we need to make sure the column names match up 

#need to change a couple things in the iso2 df
iso2 <- iso2 %>%
  rename(Trophic_Category = 'Type',
         Taxonomic_ID_Code = 'Species')

#getting rid of rock bass (311), rainbow smelt (121), slimy sculpin (382), 
#get rid of every fish iso other than 81 and 93
iso2 <- iso2 %>%
  filter(!(Taxonomic_ID_Code == "311" | Taxonomic_ID_Code == "121" | Taxonomic_ID_Code == "382"| Taxonomic_ID_Code == "199/200"| Taxonomic_ID_Code == "314"| Taxonomic_ID_Code == "313"| Taxonomic_ID_Code == "331"))


#add trophic group column
iso2 <- iso2 %>%
  mutate(Trophic_Group = case_when(
    Taxonomic_ID_Code %in% c("dragonfly", "amphipod", "isopod (mixed)", "crayfish", "insect", "caddisfly", "head of insect", "leech", "mayfly", "predatory diving beetle", "megaloptera", "predatory-water beetle") ~ "Invert",
    Taxonomic_ID_Code == "snail" ~ "Snail",
    Taxonomic_ID_Code == "81" ~ "LakeTrout",
    Taxonomic_ID_Code == "zooplankton" ~ "Zoops",
    Taxonomic_ID_Code == "93" ~ "Cisco",
    Taxonomic_ID_Code == "mussel" ~ "Mussel",
    TRUE ~ NA_character_  # If none of the conditions match, return NA
  ))

iso2 <- iso2 %>%
  rename(d13C_Cor = 'd13C_corr')

iso2 <- iso2 %>%
  select(-CN)

iso_data_sub2 <- iso_data_sub %>%
  select(Lake_Name, SI_ID, Trophic_Group, Trophic_Category, Taxonomic_ID_Code, d15N, d13C, d13C_Cor)

#now we can bind these two - iso_dat_sub3

iso_data_sub3 <- rbind(iso2, iso_data_sub2)

#check how many lakes now - should be 57
iso_data_sub_lakes3 <- iso_data_sub3 %>%
  group_by(Lake_Name) %>%
  count()
#57 lakes - looks right 
write.csv(iso_data_sub3, file="iso_dat_bsm_final.csv")







#now to add in Tom Johnstons lakes
# IMPORTANT!!! **NOTE** need to ask Tom permission to use this data if anybody wants to use it in the future 
#read in isotope data
iso_dat_tom <- read.csv("data/tom_johnston_laketrout_baseline_isotope_data.csv")

#change WBY to Lake_Name in df
iso_dat_tom <- iso_dat_tom %>%
  rename(Lake_Name = WBY)

#check how many lakes we have
lake_sum <- iso_dat_tom  %>%
  group_by(Lake_Name) %>%
  count()

#51 lakes - check if this corresponds with the list of the BSM lakes
#read in csv that has this list 

our_lakes <- read.csv("BsM_lakes_2023may27.csv")

#this has 61 lakes - guessing that a few of the ones we wanted didnt have baselines

# Find lakes not in both dfs 
common_names <- inner_join(our_lakes, lake_sum, by = "Lake_Name")

# Display common names
print(common_names$Lake_Name)

#we have 50 lakes in common - one in isotope data isnt common

# Find lakes in lake_sum not present in our_lakes
missing_lakes <- anti_join(lake_sum, our_lakes, by = "Lake_Name")

# Display missing lakes
print(missing_lakes)

#lake Nipigon is missing - remove this from the df
iso_dat_tom2 <- iso_dat_tom %>% filter(Lake_Name != "Nipigon")

#check what baselines we have 

#check what lakes have mussels and snails
mussels_snails <- iso_dat_tom2 %>%
  group_by(Lake_Name) %>%
  filter(any(TAXON == "CLAM")) %>%
  filter(any(TAXON == "SNAIL")) %>%
  filter(any(TAXON == "LT"))%>%
  filter(TAXON == "CLAM" | TAXON == "SNAIL" | TAXON == "LT")%>%
  group_by(Lake_Name, TAXON) 

#get summary of these lakes
baseline_sum <- mussels_snails  %>%
  group_by(Lake_Name) %>%
  count()

#We have 19 lakes that have both snails and mussels

#biplot of these lakes 
snail_mussel_biplots <-  mussels_snails %>%
  ggplot(aes(x = Del.13C,y = Del.15N, color = TAXON)) +
  geom_point(size = 2) +
  theme_classic() +
  xlab("d13C") + 
  ylab("d15N") + 
  facet_wrap(~Lake_Name)

snail_mussel_biplots 

##Filtering out lakes that are missing mussels
no_mussel_lakes <- iso_dat_tom2 %>%
  group_by(Lake_Name) %>%
  filter(!any(TAXON == "CLAM")) 

no_mussel_sum <- no_mussel_lakes  %>%
  group_by(Lake_Name) %>%
  count()

#check what lakes have snails 
snails_present <- iso_dat_tom2 %>%
  group_by(Lake_Name) %>%
  filter(any(TAXON == "SNAIL")) %>%
  filter(any(TAXON == "LT"))%>%
  filter(TAXON == "SNAIL" | TAXON == "LT")%>%
  group_by(Lake_Name, TAXON) 

snails_present_sum <- snails_present  %>%
  group_by(Lake_Name) %>%
  count()
#27 lakes have snail and missing mussels

missing_lakes2 <- anti_join(snails_present_sum, baseline_sum, by = "Lake_Name")

# Display missing lakes
print(missing_lakes2)

#8 lakes missing mussels that have snails that we can potentially add - Endikai, George, Johnnie, Long-1, Mesomikenda , Scotia, Trout-2, Windy

#select these 8 out of the df with snails present
snails_present <- snails_present %>%
  filter(Lake_Name %in% c("Endikai", "George", "Johnnie", "Long-1", "Mesomikenda" , "Scotia", "Trout-2", "Windy"))

#merge these lakes with the mussel_snails df - we will be able to estimate the missing mussels from baseline regressions
tom_iso_dat <- rbind(mussels_snails, snails_present)

lake_sum_tom <- tom_iso_dat  %>%
  group_by(Lake_Name) %>%
  count()
#27 lakes total 



#now to reformat this so it matches our other df
tom_iso_dat <- tom_iso_dat %>%
  rename(SI_ID = 'SIA.SAM',
         d15N = 'Del.15N',
         d13C = 'Del.13C',
         CN = 'C.N')

tom_iso_dat <- tom_iso_dat %>%
  select(Lake_Name, SI_ID,d15N,d13C, CN, TAXON)


#add trophic group column
tom_iso_dat <- tom_iso_dat %>%
  mutate(Trophic_Group = case_when(
    TAXON == "SNAIL" ~ "Snail",
    TAXON == "LT" ~ "LakeTrout",
    TAXON == "CLAM" ~ "Mussel",
    TRUE ~ NA_character_  # If none of the conditions match, return NA
  ))

#add taxonomic ID code
tom_iso_dat <- tom_iso_dat %>%
  mutate(Taxonomic_ID_Code = case_when(
    TAXON == "SNAIL" ~ "snail",
    TAXON == "LT" ~ "81",
    TAXON == "CLAM" ~ "mussel",
    TRUE ~ NA_character_  # If none of the conditions match, return NA
  ))

#add trophic category
tom_iso_dat <- tom_iso_dat %>%
  mutate(Trophic_Category = case_when(
    TAXON == "SNAIL" ~ "SNAIL",
    TAXON == "LT" ~ "PRED",
    TAXON == "CLAM" ~ "MUSSELS",
    TRUE ~ NA_character_  # If none of the conditions match, return NA
  ))


#take out TAXON column
tom_iso_dat <- tom_iso_dat %>%
  ungroup() %>%
  select(-TAXON)

#this is now in the format we want

#read in cisco data
tom_cisco <- read.csv("data/tom_johnston_cisco_isotope_data.csv")

tom_cisco <- tom_cisco %>%
  rename(SI_ID = 'SIA.SAM',
         d15N = 'Del.15N',
         d13C = 'Del.13C',
         CN = 'C.N',
         Lake_Name = 'WBY')

tom_cisco <- tom_cisco %>%
  select(Lake_Name, SI_ID,d15N,d13C, CN, TAXON)

tom_cisco <- na.omit(tom_cisco)

#check what lakes with cisco overlap with our other lakes
cisco_sum <- tom_cisco  %>%
  group_by(Lake_Name) %>%
  count()

#12 lakes with cisco total 

cisco_check <- anti_join(cisco_sum, lake_sum_tom, by = "Lake_Name")

print(cisco_check)

#only 3 lakes that arent in our df - Manitou-2, Smoothwater, Wanapitei
#take these lakes out of the cisco df

tom_cisco <- tom_cisco %>%
  filter(!(Lake_Name %in% c("Manitou-2", "Smoothwater", "Wanapitei")))

#now add some columns to this df to make it match up with the other one
#add trophic group column
tom_cisco <- tom_cisco %>%
  mutate(Trophic_Group = case_when(
    TAXON == "CISCO" ~ "Cisco",
    TRUE ~ NA_character_  # If none of the conditions match, return NA
  ))

#add taxonomic ID code
tom_cisco <- tom_cisco %>%
  mutate(Taxonomic_ID_Code = case_when(
    TAXON == "CISCO" ~ "93",
    TRUE ~ NA_character_  # If none of the conditions match, return NA
  ))

#add trophic category
tom_cisco <- tom_cisco %>%
  mutate(Trophic_Category = case_when(
    TAXON == "CISCO" ~ "PFF",
    TRUE ~ NA_character_  # If none of the conditions match, return NA
  ))


tom_cisco <- tom_cisco %>%
  ungroup() %>%
  select(-TAXON)


#now merge the cosco df with the other isotope df 

tom_iso_combined <- rbind(tom_cisco, tom_iso_dat)



##lipid correct the carbon 
##equation: dCnormalized = dCraw - 3.32 + 0.99 X C:N ---- see Post et al., 2007

lipid_corrected_dat <- tom_iso_combined %>%
  filter(Trophic_Group == "Snail" | Trophic_Group == "Mussel" | Trophic_Group == "LakeTrout"| Trophic_Group == "Cisco") %>%
  mutate(d13C_Cor = d13C - 3.32 + 0.99 * CN)


#now get rid of the CN col

tom_sio_dat_fin <- lipid_corrected_dat%>%
  select(-CN)

write.csv(tom_sio_dat_fin, file ="tom_sia_dat_final2.csv")

#added Aug 8 - making summary df to send Tom lakes we are using
lake_sum <- tom_sio_dat_fin  %>%
  group_by(Lake_Name) %>%
  count()

Tom_lakes <- tom_sio_dat_fin %>%
  select(Lake_Name)

distinct_data <- Tom_lakes %>% 
  distinct(Lake_Name, .keep_all = TRUE)

write.csv(distinct_data, file = "BsM_lakes_CassieK_Aug8_2023_2.csv")
### end of Aug 8

#now we can merge Tom's iso dat with the larger issotope df - iso_data_sub3

raw_iso_dat_final <- rbind(iso_data_sub3, tom_sio_dat_fin)

final_lakes_sum <- raw_iso_dat_final  %>%
  group_by(Lake_Name) %>%
  count()

raw_iso_dat_final <- raw_iso_dat_final %>%
  mutate(d13C = as.numeric(d13C))

write.csv(raw_iso_dat_final, file="all_raw_iso_final2.csv")

#84 lakes total

#Check how many have cisco
cisco_pres <- raw_iso_dat_final %>%
  group_by(Lake_Name) %>%
  filter(any(Trophic_Group == "Cisco")) %>%
  filter(Trophic_Group == "Cisco")%>%
  group_by(Lake_Name, Trophic_Group) 

Cisco_sum <- cisco_pres  %>%
  group_by(Lake_Name) %>%
  count()

#we have 56 lakes with cisco

#check how many lakes have snails
snail_pres <- raw_iso_dat_final %>%
  group_by(Lake_Name) %>%
  filter(any(Trophic_Group == "Snail")) %>%
  filter(Trophic_Group == "Snail")%>%
  group_by(Lake_Name, Trophic_Group) 

snail_pres_sum <- snail_pres  %>%
  group_by(Lake_Name) %>%
  count()

#78 lakes with snails 

#solution - estimate mussels off pff for lakes that have weird baselines and are missing mussels? 
check_biplots <-  raw_iso_dat_final %>%
  ggplot(aes(x = d13C,y = d15N, color = Trophic_Group)) +
  geom_point(size = 2) +
  theme_classic() +
  xlab("d13C") + 
  ylab("d15N") + 
  facet_wrap(~Lake_Name)

check_biplots

result <- raw_iso_dat_final %>%
  group_by(Lake_Name) %>%
  summarise(Trophic_Groups = paste(unique(Trophic_Group), collapse = ", "))

#lakes that we know are weird - will go back and calculate TP/LC for these separately 
#Burntroot, Three mile






















str(raw_iso_dat_final)


####BASELINE REGRESSIONS#######

##Plot regressions between mussel and pelagic forage fish -- use lakes that have both pff (cisco) and mussel
##filter out lakes that have cisco and mussels and calculate mean dC and dN


####MUSSELS VS PFF#####
#subset data
mussels_pff <- raw_iso_dat_final %>%
  group_by(Lake_Name) %>%
  filter(any(Trophic_Group == "Mussel")) %>%
  filter(any(Trophic_Group == "Cisco")) %>%
  filter(Trophic_Group == "Cisco" | Trophic_Group == "Mussel")%>%
  group_by(Lake_Name, Trophic_Group) %>%
  summarise(mean_dC = mean(d13C), 
            mean_dC_cor = mean(d13C_Cor),
            mean_dN = mean(d15N))

c_m_lake_sum <- mussels_pff  %>%
  group_by(Lake_Name) %>%
  count()

#40 lakes that have both cisco and mussels

#this is how you get the equation of line for mussels from PFF
#new df w just mussels and ratio col
df_mus <- mussels_pff %>% 
  filter(Trophic_Group == "Mussel") %>% 
  mutate(Mussel_C_N = mean_dC/mean_dN)

#new df w just cisco and ratio col
df_cis <- mussels_pff %>% 
  filter(Trophic_Group == "Cisco") %>% 
  mutate(Cisco_C_N = mean_dC/mean_dN)

#putting them back together with ratio columns 
#***Note that column names that were in both dfs have .x and .y where x is cisco and y is mussel
df_join <- inner_join(df_cis, df_mus, by = "Lake_Name")

#Plot the ratios for both groups
ratio <- ggplot(df_join, aes(x = Cisco_C_N, y = Mussel_C_N)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)+
  labs(x = "Cisco C:N", y = "Mussel C:N")

ratio

ratio_lm <- lm(Mussel_C_N ~ Cisco_C_N, data = df_join)
summary(ratio_lm)



#doing carbon now -> c_cisco v C_mussel to see if slope is 1.5 or 0.5
carbon <- ggplot(df_join, aes(x = mean_dC.x, y = mean_dC.y)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)+
  labs(x = "Cisco C", y = "Mussel C")
carbon

carbon_lm <- lm(mean_dC.y ~ mean_dC.x, data = df_join)
summary(carbon_lm)

slope <- coef(carbon_lm)[2]
slope#***Note the coefficient or slope is ~0.5



#now for N 
nitrogen <- ggplot(df_join, aes(x = mean_dN.x, y = mean_dN.y)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)+
  labs(x = "Cisco N", y = "Mussel N")
nitrogen

nitrogen_lm <- lm(mean_dN.y ~ mean_dN.x, data = df_join)
summary(nitrogen_lm)

slope <- coef(nitrogen_lm)[2]
slope#***Note the coefficient or slope is ~0.5

#####end of estimating that#####


##Calculating MUSSELS OFF PFF 
##Filtering out lakes that are missing mussels
no_mussel_lakes <- raw_iso_dat_final %>%
  group_by(Lake_Name) %>%
  filter(!any(Trophic_Group == "Mussel")) 

no_mussel_lakes_summary <- no_mussel_lakes %>%
  group_by(Lake_Name) %>%
  count()
##19 lakes
#check to see if all of these have cisco
check_biplots <-  no_mussel_lakes %>%
  ggplot(aes(x = d13C,y = d15N, color = Trophic_Group)) +
  geom_point(size = 2) +
  theme_classic() +
  xlab("d13C") + 
  ylab("d15N") + 
  facet_wrap(~Lake_Name)

check_biplots
#NO CISCO: Long-1, Scotia, Trout-2 - But they have snails - can estimate mussels off snails for these ones - take these out of this DF 
no_mussel_lakes <- no_mussel_lakes %>%
  filter(!(Lake_Name %in% c("Long-1", "Scotia", "Trout-2", "Kay")))



##need to calculate the mean d13C of PFF per lake, then use this value to run regression

#edited - used non-lipid corrected carbon
mussel_d13C <- no_mussel_lakes %>%
  group_by(Lake_Name) %>%
  filter(Trophic_Category == "PFF") %>%
  summarise(mean_d13C_pff = mean(d13C)) %>%
  mutate(d13C_mussel = 0.486*(mean_d13C_pff) - 15.495)

##add mussel value to larger dataset
baseline_data_sub <- left_join(raw_iso_dat_final, mussel_d13C, by = "Lake_Name")


##### SNAILS VS MUSSEL#####
##estimating mussels off snails
#this is how you get the equation of line for mussels from snails

#subset data
mussels_snails <- raw_iso_dat_final %>%
  group_by(Lake_Name) %>%
  filter(any(Trophic_Group == "Mussel")) %>%
  filter(any(Trophic_Group == "Snail")) %>%
  filter(Trophic_Group == "Snail" | Trophic_Group == "Mussel")%>%
  group_by(Lake_Name, Trophic_Group) %>%
  summarise(mean_dC = mean(d13C), 
            mean_dC_cor = mean(d13C_Cor),
            mean_dN = mean(d15N))


#new df w just mussels and ratio col
df_mus <- mussels_snails %>% 
  filter(Trophic_Group == "Mussel") %>% 
  mutate(Mussel_C_N = mean_dC/mean_dN)

#new df w just snails and ratio col
df_snail <- mussels_snails %>% 
  filter(Trophic_Group == "Snail") %>% 
  mutate(Snail_C_N = mean_dC/mean_dN)

#putting them back together with ratio columns 
#***Note that column names that were in both dfs have .x and .y where x is snail and y is mussel
df_muss_snail <- inner_join(df_snail, df_mus, by = "Lake_Name")

#now for N 
nitrogen <- ggplot(df_muss_snail, aes(x = mean_dN.x, y = mean_dN.y)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)+
  labs(x = "Snail N", y = "Mussel N")
nitrogen

nitrogen_lm <- lm(mean_dN.y ~ mean_dN.x, data = df_muss_snail)
summary(nitrogen_lm)

slope <- coef(nitrogen_lm)[2]
slope#***Note the coefficient or slope is ~0.5

##now carbon 
carbon <- ggplot(df_muss_snail, aes(x = mean_dC.x, y = mean_dC.y)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)+
  labs(x = "Snail C", y = "Mussel C")
carbon

carbon_lm <- lm(mean_dC.y ~ mean_dC.x, data = df_muss_snail)
summary(carbon_lm)

slope <- coef(carbon_lm)[2]
slope


#now plug the slope and intercept into equation

mussel_d15N <- no_mussel_lakes %>%
  group_by(Lake_Name) %>%
  filter(Trophic_Group == "Snail") %>%
  summarise(mean_d15N_snail = mean(d15N)) %>%
  mutate(d15N_mussel = 0.479*(mean_d15N_snail)+1.833)


baseline_data_sub1 <- left_join(baseline_data_sub, mussel_d15N, by = "Lake_Name")
#done mussel estimation 
##NOTES: for MUSSELS - carbon is estimated off cisco, nitrogen is estimated off snails
#did this for 15 lakes total - lakes not included - Kay, Long-1, Trout-2, Scotia



##ESTIMATION #2 -- SNAILS
##lakes missing snails 
no_snail_lakes <- baseline_data_sub1 %>%
  group_by(Lake_Name) %>%
  filter(!any(Trophic_Group == "Snail")) 

no_snail_lakes_summary <- no_snail_lakes %>%
  group_by(Lake_Name) %>%
  count()
##6 lakes -- 5/6 lakes have mussels (Kay don't have mussels) - take out Kay
#actually 5 lakes

no_snail_lakes <- no_snail_lakes %>%
  filter(Lake_Name != "Kay") 

##calculate d13C snail value based on existing mussels from that lake
snail_d13C <- no_snail_lakes %>%
  group_by(Lake_Name) %>%
  filter(Trophic_Group == "Mussel") %>%
  summarise(mean_d13C_mussel = mean(d13C)) %>%
  mutate(d13C_snail = ((mean_d13C_mussel) + 27.714)/0.0923)

baseline_data_sub2 <- left_join(baseline_data_sub1,
                                snail_d13C %>% dplyr::select(Lake_Name, d13C_snail), 
                                by = "Lake_Name") 

#d15N snail
snail_d15N <- no_snail_lakes %>%
  group_by(Lake_Name) %>%
  filter(Trophic_Group == "Mussel") %>%
  summarise(mean_d15N_mussel = mean(d15N)) %>%
  mutate(d15N_snail = ((mean_d15N_mussel) - 1.833)/0.478)

baseline_data_sub3 <- left_join(baseline_data_sub2, snail_d15N, by = "Lake_Name")


##clean up dataframe and only take columns we need

final_lakes_summary <- baseline_data_sub3 %>%
  group_by(Lake_Name) %>%
  count()

#take out the 4 we know are missing things 
baseline_data_sub3 <- baseline_data_sub3 %>%
  filter(!(Lake_Name %in% c("Long-1", "Scotia", "Trout-2", "Kay")))


iso_data_sub_final <- baseline_data_sub3

final_lakes_summary <- iso_data_sub_final %>%
  group_by(Lake_Name) %>%
  count()
#80 lakes total 














### note - redid this with non lipid corrected carbon 
###CALCULATING TP AND LC
##TP and % LC for lakes with both mussels and snails collected -----------------
##filtering out lakes w/ only snails and mussels
lt_snail_mussel <- iso_data_sub_final %>%
  group_by(Lake_Name) %>%
  filter(any(Trophic_Group == "Snail")) %>%
  filter(any(Trophic_Group == "Mussel"))

lt_snail_mussel_lakes <- lt_snail_mussel %>%
  group_by(Lake_Name) %>%
  count()
#60 lakes

##make biplot to see what these lakes look like
##filter data to keep just LT, snails and mussel
lt_snail_mussel_df <- lt_snail_mussel%>% 
  filter(Trophic_Group %in% c("LakeTrout", "Mussel", "Snail"))

#plot this
lt_snail_mussel_biplots <-  lt_snail_mussel_df %>%
  ggplot(aes(x = d13C,y = d15N, color = Trophic_Group)) +
  geom_point(size = 2) +
  theme_classic() +
  xlab("d13C") + 
  ylab("d15N") + 
  facet_wrap(~Lake_Name)

lt_snail_mussel_biplots 

#lakes that look weird: Burntroot, Canoe_K, three_mile
#take these out for now - will estimate mussels for them off the cisco
lt_snail_mussel <- lt_snail_mussel %>%
  filter(!(Lake_Name %in% c("Burntroot", "Canoe_K", "Three_Mile")))
####now we have 57 lakes in this df


##function to calculate TP and LC for mussel and snails baselines
BSMsm_mixing_model_pred_bound <- function(x){
  df_prep <- x
  LB <- df_prep %>%
    filter(Trophic_Group == "Snail")
  LB_mean_dC <- mean(LB$d13C)
  LB_mean_dN <- mean(LB$d15N)
  PB <- df_prep %>%
    filter(Trophic_Group == "Mussel")
  PB_mean_dC <- mean(PB$d13C)
  PB_mean_dN <- mean(PB$d15N)
  df_pred <- df_prep %>%
    filter(Trophic_Group == "LakeTrout")
  for(i in 1:nrow(df_pred)){
    LC_pred = ((df_pred$d13C - PB_mean_dC)/(LB_mean_dC - PB_mean_dC))
    LC_pred[LC_pred>1] <- 0.999
    LC_pred[LC_pred<0] <- 0.001
    TP_pred = 2 + (((LC_pred * ((df_pred$d15N - LB_mean_dN)/3.4)))+((1-LC_pred)*((df_pred$d15N - PB_mean_dN)/3.4)))
    dN_pred = df_pred$d15N - (((1-LC_pred)*PB_mean_dN)+(LC_pred*LB_mean_dN))
    LC_TP_df_pred <- cbind(LC_pred, TP_pred, dN_pred)
  }
  LC_TP_df_pred <- as.data.frame(LC_TP_df_pred)
  LC_TP_df_pred$Trophic_Group <- df_pred$Trophic_Group
  LC_TP_df_pred$Lake_Name <- df_pred$Lake_Name
  LC_TP_df_pred$SI_ID <- df_pred$SI_ID
  LC_TP_df_pred
}

tp_lc_BSMsm <- split(lt_snail_mussel, paste0(lt_snail_mussel$Lake_Name)) %>%
  map(BSMsm_mixing_model_pred_bound) %>%
  bind_rows()

##check that this matches
lt_snail_mussel_check <- tp_lc_BSMsm %>%
  group_by(Lake_Name) %>%
  count()
#57 lakes -- it matches 

##TP and %LC for lakes with snails collected and mussels estimated from regressions ------------------------
no_mussel_lakes <- iso_data_sub_final %>%
  group_by(Lake_Name) %>%
  filter(!any(Trophic_Group == "Mussel")) 

#check how many
lt_no_mussel_check <- no_mussel_lakes %>%
  group_by(Lake_Name) %>%
  count()

#15 lakes 

##make biplot to see what these baselines look like 
#filter out just mussel, snails and lt
no_mussel_lakes_df <- no_mussel_lakes %>% 
  filter(Trophic_Group %in% c("LakeTrout", "Mussel", "Snail"))

no_mussel_biplots <- no_mussel_lakes_df %>%
  ggplot(aes(x = d13C, y = d15N, color = Trophic_Group)) +
  geom_point(aes(x = d13C, y = d15N), size = 2, shape = 1) +
  geom_point(aes(x = d13C_mussel, y = d15N_mussel), size = 2, shape = 3) +
  theme_classic() +
  xlab("d13C") + 
  ylab("d15N and d15N_mussel") + 
  facet_wrap(~Lake_Name)

no_mussel_biplots 
#george, windy and Johnnie look weird - may have to take these out


BSMsm2_mixing_model_pred_bound <- function(x){
  df_prep <- x
  LB <- df_prep %>%
    filter(Trophic_Group == "Snail")
  LB_mean_dC <- mean(LB$d13C)
  LB_mean_dN <- mean(LB$d15N)
  PB_mean_dC <- mean(df_prep$d13C_mussel)
  PB_mean_dN <- mean(df_prep$d15N_mussel)
  df_pred <- df_prep %>%
    filter(Trophic_Group == "LakeTrout")
  for(i in 1:nrow(df_pred)){
    LC_pred = ((df_pred$d13C - PB_mean_dC)/(LB_mean_dC - PB_mean_dC))
    LC_pred[LC_pred>1] <- 0.999
    LC_pred[LC_pred<0] <- 0.001
    TP_pred = 2 + (((LC_pred * ((df_pred$d15N - LB_mean_dN)/3.4)))+((1-LC_pred)*((df_pred$d15N - PB_mean_dN)/3.4)))
    dN_pred = df_pred$d15N - (((1-LC_pred)*PB_mean_dN)+(LC_pred*LB_mean_dN))
    LC_TP_df_pred <- cbind(LC_pred, TP_pred, dN_pred)
  }
  LC_TP_df_pred <- as.data.frame(LC_TP_df_pred)
  LC_TP_df_pred$Trophic_Group <- df_pred$Trophic_Group
  LC_TP_df_pred$Lake_Name <- df_pred$Lake_Name
  LC_TP_df_pred$SI_ID <- df_pred$SI_ID
  LC_TP_df_pred
}


tp_lc_BSMsm2 <- split(no_mussel_lakes, paste0(no_mussel_lakes$Lake_Name)) %>%
  map(BSMsm2_mixing_model_pred_bound) %>%
  bind_rows()

#check how many
lt_no_mussel_check2 <- tp_lc_BSMsm2 %>%
  group_by(Lake_Name) %>%
  count()
#15 lakes - matches 

##TP and %LC for lakes with mussels collected and snails estimated from regressions ---------------------
no_snail_lakes <- iso_data_sub_final %>%
  group_by(Lake_Name) %>%
  filter(!any(Trophic_Group == "Snail")) 

lt_no_snail_check1 <- no_snail_lakes %>%
  group_by(Lake_Name) %>%
  count()

#5 lakes


###make biplot to see what these baselines look like with estimated snails
#filter out just mussel, snails and lt
no_snail_lakes_df <- no_snail_lakes %>% 
  filter(Trophic_Group %in% c("LakeTrout", "Mussel", "Snail"))

no_snail_biplots <- no_snail_lakes_df %>%
  ggplot(aes(x = d13C, y = d15N, color = Trophic_Group)) +
  geom_point(aes(x = d13C, y = d15N), size = 2, shape = 1) +
  geom_point(aes(x = d13C_snail, y = d15N_snail), size = 2, shape = 3) +
  theme_classic() +
  xlab("d13C") + 
  ylab("d15N") + 
  facet_wrap(~Lake_Name)


no_snail_biplots 


BSMsm3_mixing_model_pred_bound <- function(x){
  df_prep <- x
  LB_mean_dC <- mean(df_prep$d13C_snail)
  LB_mean_dN <- mean(df_prep$d15N_snail)
  PB <- df_prep %>%
    filter(Trophic_Group == "Mussel")
  PB_mean_dC <- mean(PB$d13C)
  PB_mean_dN <- mean(PB$d15N)
  df_pred <- df_prep %>%
    filter(Trophic_Group == "LakeTrout")
  for(i in 1:nrow(df_pred)){
    LC_pred = ((df_pred$d13C - PB_mean_dC)/(LB_mean_dC - PB_mean_dC))
    LC_pred[LC_pred>1] <- 0.999
    LC_pred[LC_pred<0] <- 0.001
    TP_pred = 2 + (((LC_pred * ((df_pred$d15N - LB_mean_dN)/3.4)))+((1-LC_pred)*((df_pred$d15N - PB_mean_dN)/3.4)))
    dN_pred = df_pred$d15N - (((1-LC_pred)*PB_mean_dN)+(LC_pred*LB_mean_dN))
    LC_TP_df_pred <- cbind(LC_pred, TP_pred, dN_pred)
  }
  LC_TP_df_pred <- as.data.frame(LC_TP_df_pred)
  LC_TP_df_pred$Trophic_Group <- df_pred$Trophic_Group
  LC_TP_df_pred$Lake_Name <- df_pred$Lake_Name
  LC_TP_df_pred$SI_ID <- df_pred$SI_ID
  LC_TP_df_pred
}

tp_lc_BSMsm3 <- split(no_snail_lakes, paste0(no_snail_lakes$Lake_Name)) %>%
  map(BSMsm3_mixing_model_pred_bound) %>%
  bind_rows()

lt_no_snail_check2 <- tp_lc_BSMsm3 %>%
  group_by(Lake_Name) %>%
  count()

#5 lakes - matches

##Bind all dataframes together ------------------------
tp_lc_all <- rbind(tp_lc_BSMsm, tp_lc_BSMsm2, tp_lc_BSMsm3)

tp_lc_missing <- tp_lc_all %>%
  filter(is.na(LC_pred))

check_final_dat <- tp_lc_all %>%
  group_by(Lake_Name) %>%
  count()

##77 - missing the 3 we took out

##TP LC biplots with updated data
TP_LC_biplots <- tp_lc_all %>%
  ggplot(aes(x = LC_pred, y = TP_pred, color = Trophic_Group)) +
  geom_point(size = 2) +
  theme_classic() +
  xlab("Littoral Coupling") + 
  ylab("Trophic position") + 
  facet_wrap(~Lake_Name)

TP_LC_biplots

###george, windy, Johnnie, mold look very weird - may take these out








##dealing with the weird lakes 
#lakes that look weird: Burntroot, Canoe_K, three_mile
#Mold - the snails estimated from the regression are weird - going to take out for now
#Burntroot and three mile and others - the mussels are outside the lake trout - took these out of the "lt_snail_mussel" df and recalculate them using cisco to estimate the nuew_mussel values and use this for TP/LC


##now estimate mussels off PFF for these lakes 

##start
##Calculating MUSSELS OFF PFF 
##Filtering out lakes that are missing mussels
weird_mussel_lakes <- raw_iso_dat_final %>%
  filter(Lake_Name %in% c("Burntroot", "Canoe_K", "Three_Mile")) 

weird_mussel_lakes_summary <- weird_mussel_lakes %>%
  group_by(Lake_Name) %>%
  count()




##need to calculate the mean d13C of PFF per lake, then use this value to run regression

mussel_d13C_2 <- weird_mussel_lakes %>%
  group_by(Lake_Name) %>%
  filter(Trophic_Category == "PFF") %>%
  summarise(mean_d13C_pff = mean(d13C)) %>%
  mutate(d13C_mussel_est = 0.486*(mean_d13C_pff) - 15.495)

##add mussel value to larger dataset
baseline_data_sub_wm <- left_join(raw_iso_dat_final, mussel_d13C_2, by = "Lake_Name")


##now get nitrogen 
mussel_d15N_2 <- weird_mussel_lakes %>%
  group_by(Lake_Name) %>%
  filter(Trophic_Group == "Snail") %>%
  summarise(mean_d15N_snail = mean(d15N)) %>%
  mutate(d15N_mussel_est = 0.479*(mean_d15N_snail)+1.833)



baseline_data_sub_wm2 <- left_join(mussel_d15N_2, baseline_data_sub_wm, by = "Lake_Name")

##plot these estimated ones

bt_new <-  baseline_data_sub_wm2 %>%
  ggplot(aes(x = d13C, y = d15N, color = Trophic_Group)) +
  geom_point(aes(x = d13C, y = d15N), size = 2, shape = 1) +
  geom_point(aes(x = d13C_mussel_est, y = d15N_mussel_est), size = 2, shape = 3) +
  theme_classic() +
  xlab("d13C") + 
  ylab("d15N") + 
  facet_wrap(~Lake_Name)

bt_new



BSMsm2_mixing_model_pred_bound_bt <- function(x){
  df_prep <- x
  LB <- df_prep %>%
    filter(Trophic_Group == "Snail")
  LB_mean_dC <- mean(LB$d13C)
  LB_mean_dN <- mean(LB$d15N)
  PB_mean_dC <- mean(df_prep$d13C_mussel_est)
  PB_mean_dN <- mean(df_prep$d15N_mussel_est)
  df_pred <- df_prep %>%
    filter(Trophic_Group == "LakeTrout")
  for(i in 1:nrow(df_pred)){
    LC_pred = ((df_pred$d13C - PB_mean_dC)/(LB_mean_dC - PB_mean_dC))
    LC_pred[LC_pred>1] <- 0.999
    LC_pred[LC_pred<0] <- 0.001
    TP_pred = 2 + (((LC_pred * ((df_pred$d15N - LB_mean_dN)/3.4)))+((1-LC_pred)*((df_pred$d15N - PB_mean_dN)/3.4)))
    dN_pred = df_pred$d15N - (((1-LC_pred)*PB_mean_dN)+(LC_pred*LB_mean_dN))
    LC_TP_df_pred <- cbind(LC_pred, TP_pred, dN_pred)
  }
  LC_TP_df_pred <- as.data.frame(LC_TP_df_pred)
  LC_TP_df_pred$Trophic_Group <- df_pred$Trophic_Group
  LC_TP_df_pred$Lake_Name <- df_pred$Lake_Name
  LC_TP_df_pred$SI_ID <- df_pred$SI_ID
  LC_TP_df_pred
}


tp_lc_BSM_bt <- split(baseline_data_sub_wm2, paste0(baseline_data_sub_wm2$Lake_Name)) %>%
  map(BSMsm2_mixing_model_pred_bound_bt) %>%
  bind_rows()

#now check a tp / lc biplot to see if that fixed anything 

TP_LC_biplots <- tp_lc_BSM_bt %>%
  ggplot(aes(x = LC_pred, y = TP_pred, color = Trophic_Group)) +
  geom_point(size = 2) +
  theme_classic() +
  xlab("Littoral Coupling") + 
  ylab("Trophic position") + 
  facet_wrap(~Lake_Name)

TP_LC_biplots


##now join this df with the larger TP/LC one
tp_lc_final <- rbind(tp_lc_all, tp_lc_BSM_bt)

check_final_dat2 <- tp_lc_final %>%
  group_by(Lake_Name) %>%
  count()

#80 lakes 

##TP LC biplots with updated data
TP_LC_biplots <- tp_lc_final %>%
  ggplot(aes(x = LC_pred, y = TP_pred, color = Trophic_Group)) +
  geom_point(size = 2) +
  theme_classic() +
  xlab("Littoral Coupling") + 
  ylab("Trophic position") + 
  facet_wrap(~Lake_Name)

TP_LC_biplots

###george, windy, Johnnie, mold look very weird - may take these out

#save as csv 
#write.csv(tp_lc_final, file = "final_tp_lc_July30.csv")

#write.csv(tp_lc_final, file = "final_tp_lc_July30_2.csv")

###this is the non lipid corrected version
write.csv(tp_lc_final, file = "final_tp_lc_May8.csv")

##this is the one with proper lipid correction
#write.csv(tp_lc_final, file = "final_tp_lc_July30_3.csv")
#will use this final df to calculate SIBER metrics 










#May 8/ 2025

### formation for SIBER analysis 
##SIBER for new df - this includes Tom's lakes
#NOTE: need to ask Tom permission if people want to use his data in the future
#vingette : https://cran.r-project.org/web/packages/SIBER/vignettes/Introduction-to-SIBER.html
setwd("~/Desktop/BSM")
set.seed(1)

#load packages
library(SIBER)
library(dplyr)
library(ggplot2)
library(tidyverse)


##get iso data together
#read in iso dat 

#iso_dat <- read.csv("data/final_dat/final_tp_lc_July30.csv") - reading in the non lipid corrected verison
iso_dat <- read.csv("final_tp_lc_May8.csv")

siber_df <- iso_dat
#check how many lakes are in this df
unique(siber_df$Lake_Name)

##see how many fish in each lake
fish_n <-  siber_df %>%
  group_by(Lake_Name) %>%
  mutate(sample_size = n())

#filter this df
fish_n <- fish_n %>%
  dplyr::select(Lake_Name, sample_size)

#take out birch, richardson, Indian, wabaskang, round 
##final df will have 75 lakes 
##need at least 3 individuals per group for reliable estimates - filter out lakes with less than 4 individuals

siber_df <- siber_df %>%
  filter(Lake_Name != "Birch") %>%
  filter(Lake_Name != "Indian") %>%
  filter(Lake_Name != "Richardson")%>%
  filter(Lake_Name != "Round")%>%
  filter(Lake_Name != "Wabaskang")

check_final_dat2 <- siber_df %>%
  group_by(Lake_Name) %>%
  count()

##set up data for siber - needs to be in the format c("iso1", "iso2", "group", "community")
#community - lake name
siber_df <- rename(siber_df, community = Lake_Name)

#group - species 

siber_df <- siber_df %>%
  mutate(group = case_when(
    startsWith(Trophic_Group, "LakeTrout") ~ "1"))

#iso 1/2 - n15 and c13 isotopes

siber_df1 <- siber_df %>%
  dplyr::select(LC_pred, TP_pred, group, community)

names(siber_df1)[1] <- "iso1" 
names(siber_df1)[2] <- "iso2"
##"iso1" - LC and "iso2" - TP

siber_df1$group <- as.integer(siber_df1$group)

str(siber_df1)
##Testing normal distribution
library(mvnormtest)
shapiro.test(siber_df1$iso1)
shapiro.test(siber_df1$iso2)


#create siber object
siber.obj <- createSiberObject(siber_df1)

community.hulls.args <- list(col = 1, lty = 1, lwd = 1) 
group.ellipses.args <- list(n = 100, p.interval = NULL, lty = 1, lwd = 2) ## = NULL creates Maximum Liklihood Standard Ellipse (incorporates approx. 40% of data)
group.hulls.args <- list(lty = 2, col = "grey20") 

#plot
par(mfrow = c(1,1))
plotSiberObject(siber.obj,
                ax.pad = 2,
                hulls = F, community.hulls.args = community.hulls.args,
                ellipses = T, group.ellipses.args = group.ellipses.args,
                group.hulls = F, group.hulls.args = group.hulls.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'\u2030'),
                ylab = expression({delta}^15*N~'\u2030')
)

#summary statistics for TA, SEA and SEAc
group.ML <- groupMetricsML(siber.obj)
print(group.ML)

#create df of group metrics
group_metrics_df <- as.data.frame(group.ML)

# Add row names and pivot wider
group_metrics_df <- slice(group_metrics_df, 3)

group_metrics_df <- pivot_longer(group_metrics_df, cols = everything(), names_to = "Lake_Name", values_to = "SEAc")

# Remove ".1" from Lake_Name
group_metrics_df <- group_metrics_df %>%
  mutate(Lake_Name = str_replace(Lake_Name, "\\.1$", ""))


##check if TA matches these later for individual lakes


##now calculating the laymans community metrics for lakes individually

#get list of lakes 
ID<- sort(unique(siber_df1$community))
print(ID)

##### loop for calculating laymans metrics for each lake ####


# Get list of lake names
ID <- sort(unique(siber_df1$community))

# Loop over each lake
for (lake in ID) {
  
  # Filter data for current lake
  current_lake <- siber_df1 %>%
    filter(group == "1", community == lake)
  
  # Calculate layman metrics for current lake
  current_layman <- laymanMetrics(current_lake$iso1, current_lake$iso2)
  
  # Convert layman metrics to data frame
  current_layman_df <- as.data.frame(current_layman[["metrics"]]) %>% 
    mutate(metrics = current_layman[["metrics"]]) %>% 
    dplyr::select(metrics) 
  
  # Add row names and pivot wider
  current_df <- current_layman_df %>%
    rownames_to_column(var = "rowname") %>% 
    pivot_wider(names_from = rowname, values_from = metrics)
  
  # Add lake name as column
  current_df$Lake_Name <- lake
  
  # Combine data frames for each lake
  if (lake == ID[1]) {
    combined_df <- current_df
  } else {
    combined_df <- bind_rows(combined_df, current_df)
  }
}

# View combined data frame for all lakes
combined_df

##bind this with the SEAc df 

SIBER_df_final <- merge(group_metrics_df, combined_df, by = "Lake_Name")


#write.csv(SIBER_df_final, "SIBER_July30.csv")
#write.csv(SIBER_df_final, "SIBER_July30_2.csv")



###updated using the non lipid corrected 
write.csv(SIBER_df_final, "SIBER_May8.csv")











#####May 26/ 2025 
#Analysis Prep

#read in the SIBER data 
siber_dat <- read.csv ("SIBER_May8.csv")

#read in the abiotic data
lake_dat <- read.csv("data/final_dat/abiotic_dat_july31.csv")

#read in biotic data
biotic_dat <- read.csv("data/final_dat/bioticdat_aug9.csv")

#filter out lakes
biotic_dat <- biotic_dat %>%
  filter(!Lake_Name %in% c("Indian", "Richardson", "Round", "Wabaskang", "Birch", "Kay"))


#merge these dfs by lake name
reg_df0 <- merge(lake_dat, siber_dat, by="Lake_Name")


reg_df <- full_join(reg_df0, biotic_dat, by="Lake_Name")

#df has 70 lakes in it

#take out 4 lakes that are not using in analysis
reg_df <- reg_df %>%
  filter(!Lake_Name %in% c("Mold", "Johnnie", "George", "Windy", "Endikai"))





#check the normality of the data
#start with Area_ha
shapiro.test(reg_df$Area_ha)
#p<0.05 - data not normally dist
#very left skewed - use log transformation
reg_df$log_area_ha <- log(reg_df$Area_ha)
#check histogram of transformed area_ha
hist(reg_df$log_area_ha)
#shapiro test of transformed data
shapiro.test(reg_df$log_area_ha)
#fixed 


#temp
shapiro.test(reg_df$summer_air_temp)
hist(reg_df$summer_air_temp)
#p<0.05 - data not normally dist
#very left skewed - use log transformation
reg_df$log_temp <- log(reg_df$summer_air_temp)
shapiro.test(reg_df$log_temp)
hist(reg_df$sqrt_temp)
# try sqrt transformation 
reg_df$sqrt_temp <- sqrt(reg_df$summer_air_temp)
shapiro.test(reg_df$sqrt_temp)
#didnt do anything - keep as is

#dY range 
shapiro.test(reg_df$dY_range)
reg_df$log_dY_range <- log(reg_df$dY_range)
shapiro.test(reg_df$log_dY_range)

#dX range 
shapiro.test(reg_df$dX_range)
reg_df$log_dX_range <- log(reg_df$dX_range)
shapiro.test(reg_df$log_dX_range)

#SEAc
shapiro.test(reg_df$SEAc)
reg_df$log_SEAc <- log(reg_df$SEAc)
shapiro.test(reg_df$log_SEAc)

#LaTro_CUE
shapiro.test(reg_df$LaTro_CUE)
reg_df$log_LaTro_CUE <- log(reg_df$LaTro_CUE)
shapiro.test(reg_df$log_LaTro_CUE)

#SmBas_CUE
shapiro.test(reg_df$SmBas_CUE)
reg_df$log_SmBas_CUE <- log(reg_df$SmBas_CUE)
shapiro.test(reg_df$log_SmBas_CUE)
#didnt do anything --- because there are zeros and cant log a zero
#replace zeros with 0.01
reg_df <- reg_df %>%
  mutate(SmBas_CUE = ifelse(SmBas_CUE == 0, 0.01, SmBas_CUE))
reg_df$log_SmBas_CUE <- log(reg_df$SmBas_CUE)
shapiro.test(reg_df$log_SmBas_CUE)
hist(reg_df$log_SmBas_CUE)
#try sqrt
reg_df$sqrt_SmBas_CUE <- sqrt(reg_df$SmBas_CUE)
shapiro.test(reg_df$sqrt_SmBas_CUE)

#shannon div index
shapiro.test(reg_df$div_shannon)
#normal

#NNd
shapiro.test(reg_df$NND)
reg_df$log_NND <- log(reg_df$NND)
shapiro.test(reg_df$log_NND)

#SDNNd
shapiro.test(reg_df$SDNND)
reg_df$log_SDNND <- log(reg_df$SDNND)
shapiro.test(reg_df$log_SDNND)

#CD
shapiro.test(reg_df$CD)
reg_df$log_CD <- log(reg_df$CD)
shapiro.test(reg_df$log_CD)

#total pred cue
shapiro.test(reg_df$total_pred_CUE)
reg_df$log_total_pred_CUE <- log(reg_df$total_pred_CUE)
shapiro.test(reg_df$log_total_pred_CUE)


#transformations - log: area, dy rnage, dx range, lake area, seac, LT CPUE, NND, sdnnd, cd
#summer air temp, smb cpue - transformation didnt work
#Shannon div index - no trans - it is already normal



## lets filter some stuff for the final dataset for analysis 
#first take out lat and long

##filter out lakes name and lat and long 

reg_df <- reg_df %>%
  select(-X.x) 

reg_df <- reg_df %>%
  select(-X.y, -X, -log_temp, -sqrt_temp)

reg_df <- reg_df %>%
  select(-Latitude, -Longitude)

reg_df <- reg_df %>%
  select(-div_shannon, -SmBas_CUE, -log_SmBas_CUE, -sqrt_SmBas_CUE)

##save this data as a csv
write.csv(reg_df, file="bsm_analysis_datmay26.csv")












####now run analysis with non lipid corrected data

#read in the data
reg_df <- read.csv ("bsm_analysis_datmay26.csv")

##run regression models for trophic position range, littoral coupling range, SEAc and SDNND

#Trophic position range (dY range)
tpr_mlr1 <- lm(log_dY_range ~ log_area_ha + summer_air_temp, data = reg_df)

summary(tpr_mlr1)


#make partial regression plots for TP range 
#model with just area
model_area_tp<- lm(log_dY_range ~ log_area_ha, data = reg_df)
reg_df$tp_area_residuals <- residuals(model_area_tp) 

#model with just temp
model_temp_tp <- lm(log_dY_range ~ summer_air_temp, data = reg_df)
reg_df$temp_residuals_tp <- residuals(model_temp_tp)

#temp against area
model_temp_against_area <- lm(summer_air_temp ~ log_area_ha, data = reg_df)
reg_df$temp_against_area_residuals <- residuals(model_temp_against_area) #

#model area against temp
model_area_against_temp <- lm(log_area_ha ~ summer_air_temp, data = reg_df)
reg_df$area_against_temp_residuals <- residuals(model_area_against_temp) 

##Area vs tp plot
TP_area_plot <- ggplot(reg_df, aes(x = area_against_temp_residuals, y = temp_residuals_tp)) +
  geom_point() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme_classic() +
  theme(axis.text = element_text(size = 16))+
  xlab("Lake Area (log(ha))") +
  ylab("Trophic Position Range (log)")
TP_area_plot

#temp vs tp plot
TP_temp_plot <- ggplot(reg_df, aes(x = temp_against_area_residuals, y = tp_area_residuals)) +
  geom_point() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme_classic() +
  theme(axis.text = element_text(size = 16))+
  xlab("Summer Air Temperature (C)") +
  ylab("Trophic Posiiton Range (log)")
TP_temp_plot



#Littoral coupling range (dX range) 
reg_df <- reg_df %>%
  filter(is.finite(log_dX_range))

lc_mlr1 <- lm(log_dX_range ~ log_area_ha + summer_air_temp, data = reg_df)

summary(lc_mlr1)


#Partial regression plots

#model with just area
model_area_lc <- lm(log_dX_range ~ log_area_ha, data = reg_df)
reg_df$lc_area_residuals <- residuals(model_area_lc) 

#model with just temp
model_temp <- lm(log_dX_range ~ summer_air_temp, data = reg_df)
reg_df$temp_residuals <- residuals(model_temp)

#temp against area
model_temp_against_area <- lm(summer_air_temp ~ log_area_ha, data = reg_df)
reg_df$temp_against_area_residuals <- residuals(model_temp_against_area) #

#model area against temp
model_area_against_temp <- lm(log_area_ha ~ summer_air_temp, data = reg_df)
reg_df$area_against_temp_residuals <- residuals(model_area_against_temp) 


##LC~area
LC_area_plot2 <- ggplot(reg_df, aes(x = area_against_temp_residuals, y = temp_residuals)) +
  geom_point() + geom_smooth(method = 'lm', color = '#e76254') +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme_classic() +
  theme(axis.text = element_text(size = 16))+
  xlab("Lake Area (log(ha))") +
  ylab("Littoral Coupling Range (log)")
LC_area_plot2


#LC~temp
LC_temp_plot2 <- ggplot(reg_df, aes(x = temp_against_area_residuals, y = lc_area_residuals)) +
  geom_point() + geom_smooth(method = 'lm', color = '#e76254') +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme_classic() +
  theme(axis.text = element_text(size = 16))+
  xlab("Summer Air Temperature (C)") +
  ylab("Littoral Coupling Range (log)")
LC_temp_plot2





#SEAc - standard ellipse area
seac_mlr1 <- lm(log_SEAc ~ log_area_ha + summer_air_temp, data = reg_df)

summary(seac_mlr1)

###partial regression plots 
#model with just area
model_area_seac <- lm(log_SEAc ~ log_area_ha, data = reg_df)
reg_df$seac_area_residuals <- residuals(model_area_seac) 

#model with just temp
model_temp_seac <- lm(log_SEAc ~ summer_air_temp, data = reg_df)
reg_df$seac_temp_residuals <- residuals(model_temp_seac)

#temp against area
model_temp_against_area <- lm(summer_air_temp ~ log_area_ha, data = reg_df)
reg_df$temp_against_area_residuals <- residuals(model_temp_against_area) #

#model area against temp
model_area_against_temp <- lm(log_area_ha ~ summer_air_temp, data = reg_df)
reg_df$area_against_temp_residuals <- residuals(model_area_against_temp) 

#area vs seac
seac_area_plot <- ggplot(reg_df, aes(x = area_against_temp_residuals, y = seac_temp_residuals)) +
  geom_point() + geom_smooth(method = 'lm', color = '#e76254')  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme_classic() +
  theme(axis.text = element_text(size = 16))+
  xlab("Lake Area (log(ha))") +
  ylab("SEAc (log)")
seac_area_plot


#seac vs temp plot
seac_temp_plot <- ggplot(reg_df, aes(x = temp_against_area_residuals, y = seac_area_residuals)) +
  geom_point() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme_classic() +
  theme(axis.text = element_text(size = 16))+
  xlab(" Summer Air Temp (C)") +
  ylab("SEAc (log)")


seac_temp_plot


#SDNND - standard deviation of nearest neighbor distance
sdnnd_mlr1 <- lm(log_SDNND ~ log_area_ha + summer_air_temp, data = reg_df)

summary(sdnnd_mlr1)


###partial plots for SDNND 
#model with just area
model_area_sdnnd <- lm(log_SDNND ~ log_area_ha, data = reg_df)
reg_df$sdnnd_area_residuals <- residuals(model_area_sdnnd) 

#model with just temp
model_temp_sdnnd <- lm(log_SDNND ~ summer_air_temp, data = reg_df)
reg_df$sdnnd_temp_residuals <- residuals(model_temp_sdnnd)

#temp against area
model_temp_against_area <- lm(summer_air_temp ~ log_area_ha, data = reg_df)
reg_df$temp_against_area_residuals <- residuals(model_temp_against_area) #

#model area against temp
model_area_against_temp <- lm(log_area_ha ~ summer_air_temp, data = reg_df)
reg_df$area_against_temp_residuals <- residuals(model_area_against_temp) 

#area vs sdnnd
sdnnd_area_plot <- ggplot(reg_df, aes(x = area_against_temp_residuals, y = sdnnd_temp_residuals)) +
  geom_point() + geom_smooth(method = 'lm', color = '#e76254') +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme_classic() +
  theme(axis.text = element_text(size = 16))+
  xlab("Lake Area (log(ha))") +
  ylab("SDNND(log)")
sdnnd_area_plot


#sdnnd vs temp plot
sdnnd_temp_plot <- ggplot(reg_df, aes(x = temp_against_area_residuals, y = sdnnd_area_residuals)) +
  geom_point() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme_classic() +
  theme(axis.text = element_text(size = 16))+
  xlab(" Summer Air Temp (C)") +
  ylab("SDNND (log)")


sdnnd_temp_plot















