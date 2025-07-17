##SIBER analysis with updated data
#March 28/2023

setwd("~/Desktop/BSM")
set.seed(1)

#load packages
library(SIBER)
library(dplyr)
library(ggplot2)
library(tidyverse)


##get iso data together
#source scripts with BSM tp and lc dfs
source("code/Baseline_Regressions.R")

#iso dat from Baseline_Regressions.R
tp_lc_all

siber_df <- tp_lc_all
#check how many lakes are in this df
unique(siber_df$Lake_Name)

##need at least 3 individuals per group for reliable estimates - filter out lakes with less than 4 individuals

siber_df <- siber_df %>%
  filter(Lake_Name != "Birch") %>%
  filter(Lake_Name != "Indian") %>%
  filter(Lake_Name != "Richardson")%>%
  filter(Lake_Name != "Round")%>%
  filter(Lake_Name != "Wabaskang")%>%
  filter(Lake_Name != "Sekulmun")

##set up data for siber - needs to be in the format c("iso1", "iso2", "group", "community")
#community - lake name
siber_df <- rename(siber_df, community = Lake_Name)

#group - species 

siber_df <- siber_df %>%
  mutate(group = case_when(
    startsWith(Trophic_Group, "LakeTrout") ~ "1"))

#iso 1/2 - n15 and c13 isotopes

siber_df1 <- siber_df %>%
  select(LC_pred, TP_pred, group, community)

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
##check if TA matches these later for individual lakes


##now calculating the laymans community metrics for lakes individually
#go off this code: 
Abbess <- siber_df1 %>%
  filter(group == "1", community == "Abbess")

Abbess_layman <-laymanMetrics(Abbess$iso1, Abbess$iso2)
Abbess_layman <- as.data.frame(Abbess_layman) %>% 
  select(metrics) 


df_with_rownames <- Abbess_layman %>%
  rownames_to_column(var = "rowname")%>% 
  pivot_wider(names_from = rowname, values_from = metrics)

df_with_rownames$Lake_Name <- "Abbess"

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
    select(metrics) 
  
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


#final df has 56 lakes - we took some out because of SIBER needing at least 3 values per lake
#something weird going on with burntroot
#something weird with Sekulmun
#These TA values match the earlier TA values from the group metrics

##make histograms
ggplot(combined_df, aes(dX_range)) +
  geom_histogram()

ggplot(combined_df, aes(dY_range)) +
  geom_histogram()

##analysis - lake traits vs laymans metrics

lakedat <- read.csv("data/cassie_bsm_M28.csv")

#merge laymans metrics data and lake trait data
lake_layman_df<- merge(lakedat, combined_df, by = "Lake_Name")
#54 lakes total in this df


###DO THIS LATER
##check normality
#normality test - shapiro wilk 
#area_ha
shapiro.test(lake_layman_df$Area_ha)
#p<0.05 - data not normally dist
#check with histogram to see 
hist(lake_layman_df$Area_ha)
##right skewed

#perimeter_km
shapiro.test(lake_layman_df$Perimeter_km)
hist(lake_layman_df$Perimeter_km)
#log transform

#Summer air temp 
shapiro.test(lake_layman_df$Summer_Air_Temp)
hist(lake_layman_df$Summer_Air_Temp)
#log

#pLittoral
shapiro.test(lake_layman_df$pLittoral)
hist(lake_layman_df$pLittoral)

#pLittoral 4m
shapiro.test(lake_layman_df$pLittoral_4m)

#Dev_Index
shapiro.test(lake_layman_df$Dev_Index)
hist(lake_layman_df$Dev_Index)

#TA
shapiro.test(lake_layman_df$TA)
hist(lake_layman_df$TA)
#log

#dY_range
shapiro.test(lake_layman_df$dY_range)
hist(lake_layman_df$dY_range)
#log

#dX_range
shapiro.test(lake_layman_df$dX_range)
hist(lake_layman_df$dX_range)
#log

#CD
shapiro.test(lake_layman_df$CD)
hist(lake_layman_df$CD)
#log

#NND
shapiro.test(lake_layman_df$NND)
hist(lake_layman_df$NND)
#log

#SDNND
shapiro.test(lake_layman_df$SDNND)
hist(lake_layman_df$SDNND)
#log

#transformations
lake_layman_df <- lake_layman_df %>%
  mutate(Area_ha_log = log(Area_ha)) %>%
  mutate(Perimeter_km_log = log(Perimeter_km))%>%
  mutate(Summer_Air_Temp_log = log(Summer_Air_Temp))%>%
  mutate(pLittoral_logit = logit(pLittoral))%>%
  mutate(Dev_Index_log = log(Dev_Index)) %>%
  mutate(TA_log = log(TA)) %>%
  mutate(dY_range_log = log(dY_range))%>%
  mutate(dX_range_log = log(dX_range)) %>%
  mutate(CD_log = log(CD))%>%
  mutate(NND_log = log(NND))%>%
  mutate(SDNND_log = log(SDNND))

#check distribution again
hist(lake_layman_df$Area_ha_log)
hist(lake_layman_df$Perimeter_km_log)
hist(lake_layman_df$Summer_Air_Temp_log)
hist(lake_layman_df$pLittoral_logit)
hist(lake_layman_df$Dev_Index)
hist(lake_layman_df$TA_log)  
hist(lake_layman_df$dY_range_log)  
hist(lake_layman_df$dX_range_log) 
#still not the best ^
hist(lake_layman_df$CD_log) 
hist(lake_layman_df$NND_log) 
hist(lake_layman_df$SDNND_log) 

#some weird stuff going on with burntroot and three mile
##take out for now
lake_layman_df <- lake_layman_df %>%
  filter(Lake_Name != "Burntroot") %>%
  filter(Lake_Name != "Three_Mile") 

#run regressions 
#start with TA
#area_ha vs TA
ta_area_lm <- lm(TA_log ~ Area_ha_log, lake_layman_df)
summary(ta_area_lm)


##plot
ta_area_plot <- ggplot(lake_layman_df, aes(Area_ha_log, TA_log)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic()
ta_area_plot

#perimeter vs TA
ta_per_lm <- lm(TA_log ~ Perimeter_km_log, lake_layman_df)
summary(ta_per_lm)


##plot
ta_per_plot <- ggplot(lake_layman_df, aes(Perimeter_km_log, TA_log)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic()
ta_per_plot

#pLittoral
ta_pl_lm <- lm(TA_log ~ pLittoral_logit, lake_layman_df)
summary(ta_pl_lm)


##plot
ta_pl_plot <- ggplot(lake_layman_df, aes(pLittoral_logit, TA_log)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic()
ta_pl_plot

#pLitt 4 m
ta_pl_lm <- lm(TA_log ~ pLittoral_4m, lake_layman_df)
summary(ta_pl_lm)

##plot
ta_pl_plot <- ggplot(lake_layman_df, aes(pLittoral_4m, TA_log)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic()
ta_pl_plot

#Dev Index
ta_di_lm <- lm(TA_log ~ Dev_Index_log, lake_layman_df)
summary(ta_di_lm)


##plot
ta_di_plot <- ggplot(lake_layman_df, aes(Dev_Index_log, TA_log)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic()
ta_di_plot

###
#Summer air temp
ta_temp_lm <- lm(TA_log ~ Summer_Air_Temp_log, lake_layman_df)
summary(ta_temp_lm)


##plot
ta_temp_plot <- ggplot(lake_layman_df, aes(Summer_Air_Temp_log, TA_log)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic()
ta_temp_plot

##dY_range

#area_ha vs dY_range
dY_range_area_lm <- lm(dY_range_log ~ Area_ha_log, lake_layman_df)
summary(dY_range_area_lm)


##plot
dY_range_area_plot <- ggplot(lake_layman_df, aes(Area_ha_log, dY_range_log)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic()
dY_range_area_plot

#perimeter vs dY_range
dY_range_per_lm <- lm(dY_range_log ~ Perimeter_km_log, lake_layman_df)
summary(dY_range_per_lm)


##plot
dY_range_per_plot <- ggplot(lake_layman_df, aes(Perimeter_km_log, dY_range_log)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic()
dY_range_per_plot

#pLittoral
dY_range_pl_lm <- lm(dY_range_log ~ pLittoral_logit, lake_layman_df)
summary(dY_range_pl_lm)


##plot
dY_range_pl_plot <- ggplot(lake_layman_df, aes(pLittoral_logit, dY_range_log)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic()
dY_range_pl_plot

#pLittoral 4 m
dY_range_pl_lm <- lm(dY_range_log ~ pLittoral_4m, lake_layman_df)
summary(dY_range_pl_lm)


##plot
dY_range_pl_plot <- ggplot(lake_layman_df, aes(pLittoral_4m, dY_range_log)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic()
dY_range_pl_plot

#Dev Index
dY_range_di_lm <- lm(dY_range_log ~ Dev_Index_log, lake_layman_df)
summary(dY_range_di_lm)


##plot
dY_range_di_plot <- ggplot(lake_layman_df, aes(Dev_Index_log, dY_range_log)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic()
dY_range_di_plot

#Summer air temp
dY_range_temp_lm <- lm(dY_range_log ~ Summer_Air_Temp_log, lake_layman_df)
summary(dY_range_temp_lm)


##plot
dY_range_temp_plot <- ggplot(lake_layman_df, aes(Summer_Air_Temp_log, dY_range_log)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic()
dY_range_temp_plot

##dX_range##
#area_ha vs dX_range
dX_range_area_lm <- lm(dX_range_log ~ Area_ha_log, lake_layman_df)
summary(dX_range_area_lm)


##plot
dX_range_area_plot <- ggplot(lake_layman_df, aes(Area_ha_log, dX_range_log)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic()
dX_range_area_plot

#perimeter vs dX_range
dX_range_per_lm <- lm(dX_range_log ~ Perimeter_km_log, lake_layman_df)
summary(dX_range_per_lm)


##plot
dX_range_per_plot <- ggplot(lake_layman_df, aes(Perimeter_km_log, dX_range_log)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic()
dX_range_per_plot

#pLittoral
dX_range_pl_lm <- lm(dX_range_log ~ pLittoral_logit, lake_layman_df)
summary(dX_range_pl_lm)


##plot

dX_range_pl_plot <- ggplot(lake_layman_df, aes(pLittoral_logit, dX_range_log)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic()
dX_range_pl_plot

##pLittoral 4m

dX_range_pl_lm <- lm(dX_range_log ~ pLittoral_4m, lake_layman_df)
summary(dX_range_pl_lm)


##plot

dX_range_pl_plot <- ggplot(lake_layman_df, aes(pLittoral_4m, dX_range_log)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic()
dX_range_pl_plot
#Dev Index
dX_range_di_lm <- lm(dX_range_log ~ Dev_Index_log, lake_layman_df)
summary(dX_range_di_lm)


##plot
dX_range_di_plot <- ggplot(lake_layman_df, aes(Dev_Index_log, dX_range_log)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic()
dX_range_di_plot

#Summer air temp
dX_range_temp_lm <- lm(dX_range_log ~ Summer_Air_Temp_log, lake_layman_df)
summary(dX_range_temp_lm)


##plot
dX_range_temp_plot <- ggplot(lake_layman_df, aes(Summer_Air_Temp_log, dX_range_log)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic()
dX_range_temp_plot

##CD##
#area_ha vs CD
CD_area_lm <- lm(CD_log ~ Area_ha_log, lake_layman_df)
summary(CD_area_lm)


##plot
CD_area_plot <- ggplot(lake_layman_df, aes(Area_ha_log, CD_log)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic()
CD_area_plot

#perimeter vs CD
CD_per_lm <- lm(CD_log ~ Perimeter_km_log, lake_layman_df)
summary(CD_per_lm)


##plot
CD_per_plot <- ggplot(lake_layman_df, aes(Perimeter_km_log, CD_log)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic()
CD_per_plot

#pLittoral
CD_pl_lm <- lm(CD_log ~ pLittoral_logit, lake_layman_df)
summary(CD_pl_lm)


##plot
CD_pl_plot <- ggplot(lake_layman_df, aes(pLittoral_logit, CD_log)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic()
CD_pl_plot

#Dev Index
CD_di_lm <- lm(CD_log ~ Dev_Index_log, lake_layman_df)
summary(CD_di_lm)


##plot
CD_di_plot <- ggplot(lake_layman_df, aes(Dev_Index_log, CD_log)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic()
CD_di_plot

#Summer air temp
CD_temp_lm <- lm(CD_log ~ Summer_Air_Temp_log, lake_layman_df)
summary(CD_temp_lm)