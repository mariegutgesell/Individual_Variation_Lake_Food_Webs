##Map for Supplement -- Ecology Revision 1 
##Compare temperature by latitude/longitude -- for response letter comment R2-33
##July 1, 2025

library(tidyverse)
library(mapview)
library(sf)
library(ggpubr)


##Read in lake attribute table -- this is the updated table of final 52 lakes based on updated analysis for Revision 1 - put together by Cassie
lake_df <- read.csv("data/lake_supp_table.csv")

##list of 28 lakes w/ cpue data and used for main analysis:
#[1] "Atikwa"               "Aylen"                "Big_Rideaux"          "Bluffpoint"
#[5] "Cecil"                "Charleston"           "Cliff"                "Confusion"
#[9] "Cross"                "Delaney"              "Dogpaw"               "Dryberry"
#[13] "Eva"                  "Kaiashkons"           "Kakagi"               "Katimiagamak"
#[17] "Lake_of_Bays"         "Longlegged"           "Lower_Manitou"        "Pickerel"
#[21] "Rosseau"              "Silver"               "Stormy"               "Tadpole"
#[25] "Upper_Manitou"        "Upper_Medicine_Stone" "Victoria"             "Wapageisi"


##check for 28 lakes w/ cpue data
cpue_lakes <- lake_df %>%
  filter(!is.na(Lake.Trout.CUE))

##Add column indicating if site used in main analysis 
lake_df <- lake_df %>%
  mutate(Main_Analysis = case_when(
    is.na(Lake.Trout.CUE) ~ "No",
    !is.na(Lake.Trout.CUE) ~ "Yes",
  ))


##Make site map, color by used in main analysis 

sites_coord <- st_as_sf(lake_df, coords = c("Longitude", "Latitude"), crs = 4326)
mapview(sites_coord, zcol = "Main_Analysis", col.regions = c("grey", "red"), legend = TRUE, map.types = "Esri.WorldImagery")

mapview(sites_coord, zcol = "Main_Analysis", col.regions = c("black", "blue"), legend = TRUE)

mapview(sites_coord, zcol = "Main_Analysis", col.regions = c("black", "blue"), legend = TRUE, map.types = "Esri.NatGeoWorldMap")






##look at differences in temp across latitude/longitude cluster
lat_lm <- ggplot(lake_df, aes(x = Latitude, y = Summer.Air.Temperature..C.)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() +
  ylab("Summer Air Temperature (˚C)") 
lat_lm  

ggplot(lake_df, aes(x = Longitude, y = Summer.Air.Temperature..C.)) +
  geom_point()


lake_df <- lake_df %>%
  mutate(longitude_cluster = case_when(
         Longitude < -90 ~ "Longitude < -90˚",
         Longitude > -90 ~ "Longitude > -90˚",
         ))

long_box <- ggplot(lake_df, aes(x = longitude_cluster, y = Summer.Air.Temperature..C.)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Summer Air Temperature (˚C)") +
  xlab("")
long_box

long_temp_aov <- aov(Summer.Air.Temperature..C. ~ longitude_cluster, data = lake_df)
summary(long_temp_aov)  
  
lat_temp_lm <- lm(Summer.Air.Temperature..C. ~ Latitude, data = lake_df)
summary(lat_temp_lm)


fig_r2_33 <- ggarrange(long_box, lat_lm, nrow = 2, ncol = 1, labels = c("a)", "b)"))
fig_r2_33
