##Looking into trophic results 
library(mgcv)

source("marie_code/raw_si_df_formation.R")

rm(list = ls()[!ls() %in% c("raw_si_df")])

##raw_si_df has 66 lakes, will with cisco present - which includes all 53 bsm lakes, 4 extra lakes, and 9 lakes from Tom that have cisco SI data (assuming SI data means CS present) -- bu
##so all of tylers/tims lake had cisco -- based on the Data for Export tab in Tims master excel sheet, have numbers of SI samples for cisco (indicating they are present) or presence/absence says present - so should include all 53 based on that 
##don't necessarily need to have the cisco data, but know that they were present (but keep cisco where they are)
##do the 4 extra lakes also have cisco? 

##looking at C:N ratios 
c_n <- raw_si_df %>%
  filter(Trophic_Group == "LakeTrout")

##lots of C:N ratios below 4 for lake trout (over 90%) - so doesn't really give us good justification to lipid correct - so lets take a look at the baseline regression approach, and if using other baselines from same site may help


si_sub <- raw_si_df %>%
  filter(Trophic_Group %in% c("LakeTrout", "Cisco", "Snail", "Mussel", "Zoops", "Invert"))

test1 <- si_sub %>%
  select(Lake_Name) %>%
  unique()


ggplot(si_sub, aes(x = d13C, y = d15N, color = Trophic_Group)) +
  geom_point(size = 2) +
  theme_classic() +
  xlab("d13C") + 
  ylab("d15N") +
  facet_wrap(~Lake_Name)


##Filtering baselines 
# First, create a lookup table of taxon groups present in each lake
lake_baseline_taxa <- si_sub %>%
  filter(!Trophic_Group %in% c("LakeTrout", "Cisco")) %>%
  group_by(Lake_Name) %>%
  summarise(taxa = list(unique(Trophic_Group)), .groups = "drop")

# Now, define a function to return which taxa to keep based on the rules
decide_taxa <- function(taxa) {
  if (all(c("Snail", "Mussel") %in% taxa)) {
    return(c("Snail", "Mussel"))
  } else if ("Snail" %in% taxa & "Zoops" %in% taxa & !"Mussel" %in% taxa) {
    return(c("Snail", "Zoops"))
   } else if ("Mussel" %in% taxa & "Invert" %in% taxa & !"Snail" %in% taxa) {
      return(c("Mussel", "Invert")) 
   } else if ("Snail" %in% taxa & "Inverts" %in% taxa & !"Mussel" %in% taxa & !"Zoops" %in% taxa){
   return("Snail")
 }  else if (!"Snail" %in% taxa & !"Inverts" %in% taxa & "Mussel" %in% taxa & "Zoops" %in% taxa){
  return("Mussel")
} else {
    return(taxa)  # default: keep all
  }
}


# Apply the function to generate the taxa to keep per lake
lake_baseline_taxa <- lake_baseline_taxa %>%
  mutate(keep = lapply(taxa, decide_taxa)) %>%
  unnest_longer(keep)

# Unnest and filter the original dataframe
baseline_df <- si_sub %>%
  inner_join(lake_baseline_taxa, by = c("Lake_Name" = "Lake_Name", "Trophic_Group" = "keep")) %>%
  select(-taxa)
  
lt_cs_df <- si_sub %>%
  filter(Trophic_Group %in% c("LakeTrout", "Cisco"))

final_si_df <- rbind(baseline_df, lt_cs_df)


required_taxa <- c("Snail", "Mussel")

sites_with_m_s <- baseline_df %>%
  #  distinct(WBY, TAXON) %>%     # remove duplicate taxa per site
  group_by(Lake_Name) %>%
  summarize(has_all = all(required_taxa %in% Trophic_Group)) %>%
  filter(has_all) %>%
  pull(Lake_Name)

# Filter original dataframe to just those sites
sites_with_m_s_df <- baseline_df %>%
  filter(Lake_Name %in% sites_with_m_s) %>%
  select(Lake_Name) %>%
  unique()


lakes_with_less5_LT <- lt_cs_df %>%
  filter(Trophic_Group == "LakeTrout") %>%
  count(Lake_Name) %>%
  filter(n < 4)



lakes_with_8_LT <- final_si_df %>%
  filter(Trophic_Group == "LakeTrout") %>%
  count(Lake_Name) %>%
  filter(n >= 5) %>%
  filter(Lake_Name %in% sites_with_m_s_df$Lake_Name)

final_si_df <- final_si_df %>%
  filter(Lake_Name %in% lakes_with_8_LT$Lake_Name) 


test <- final_si_df %>%
  select(Lake_Name) %>%
  unique()
##75 lakes, with cisco and at least 4 LT 
##only 53 of these have both snails and mussels 
75-8 = 67 
##how many of these lakes have only 1 baseline habitat? 

ggplot(final_si_df, aes(x = d13C, y = d15N, color = Trophic_Group)) +
  geom_point(size = 2) +
  theme_classic() +
  xlab("d13C") + 
  ylab("d15N") +
  facet_wrap(~Lake_Name)


##Lakes with only 1 baseline: George, Johnnie Mesomikenda, -- all only have snails, would just need to estimate pelagic baseline 



## 




final_si_df <-final_si_df %>%
  mutate(trophic_type = case_when(
    startsWith(Trophic_Group, "Snail") ~ "Littoral Baseline", 
    startsWith(Trophic_Group, "Invert") ~ "Littoral Baseline", 
    startsWith(Trophic_Group, "Mussel") ~ "Pelagic Baseline", 
    startsWith(Trophic_Group, "Zoops") ~ "Pelagic Baseline", 
    startsWith(Trophic_Group, "LakeTrout") ~ "LakeTrout", 
    startsWith(Trophic_Group, "Cisco") ~ "Cisco", 
  )) %>%
  filter(Trophic_Group != "Cisco") %>%
  group_by(Lake_Name) %>%
  filter(all(c("Littoral Baseline", "Pelagic Baseline") %in% trophic_type)) %>%
  ungroup()

test3 <- final_si_df %>%
  select(Lake_Name) %>%
  unique()
##65 lakes with both baselines , at least 5 LT 

ggplot(final_si_df, aes(x = d13C_Cor, y = d15N, color = Trophic_Group)) +
  geom_point(size = 2) +
  theme_classic() +
  xlab("d13C_Cor") + 
  ylab("d15N") +
  facet_wrap(~Lake_Name)

ggplot(final_si_df, aes(x = d13C, y = d15N, color = Trophic_Group)) +
  geom_point(size = 2) +
  theme_classic() +
  xlab("d13C") + 
  ylab("d15N") +
  facet_wrap(~Lake_Name)

ggplot(final_si_df, aes(x = d13C_Cor, y = d13C, color = trophic_type)) +
  geom_point(size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray")+
  theme_classic() +
  xlab("d13C_Cor") + 
  ylab("d13C") +
  facet_wrap(~Lake_Name)

ggplot() +
  geom_point(data = final_si_df, aes(x = d13C, y = d15N, color = trophic_type), shape = "circle") +
  geom_point(data = final_si_df, aes(x = d13C_Cor, y = d15N), shape = "triangle") +
  theme_classic() +
  facet_wrap(~Lake_Name)

##lakes where d13C_Cor and d13C aren't perfectly correlated: Argo,  Burntroot, Catfish, Dryberry, Hogan, Three_Mile, White_Partridge
final_si_df_2 <- final_si_df %>%
  filter(!Lake_Name %in% c("Catfish", "Dryberry", "Hogan", "Three_Mile", "White_Partridge"))

##lakes with no baseline separation: burntroot, canoe, hogan, kakagi, kay, mendelssohn, three_mile, 
#final_si_df <- final_si_df %>%
#  filter(!Lake_Name %in% c("Burntroot", "Canoe", "Hogan", "Kakagi", "Kay", "Mendelssohn", "Three_Mile"))

##some sites where littoral baselines are on top of pelagic baselines
##is there any logic, or anything in taking the baseline with the furthest d13C sig? 

filtered_df <- final_si_df %>%
 group_by(Lake_Name, trophic_type) %>%
  mutate(
   d13C_min = min(d13C, na.rm = TRUE),
   d13C_max = max(d13C, na.rm = TRUE),
    n_type = n()
  ) %>%
  ungroup() %>%
  filter(
   (trophic_type == "Pelagic Baseline" & (n_type == 1 | d13C == d13C_min)) |
     (trophic_type == "Littoral Baseline" & (n_type == 1 | d13C == d13C_max)) |
      trophic_type == "LakeTrout"
  ) %>%
  select(-d13C_min, -d13C_max, -n_type)

ggplot(filtered_df, aes(x = d13C, y = d15N, color = trophic_type)) +
  geom_point(size = 2) +
  theme_classic() +
  xlab("d13C_Cor") + 
  ylab("d15N") +
  facet_wrap(~Lake_Name)

ggplot(filtered_df, aes(x = d13C_Cor, y = d15N, color = trophic_type)) +
  geom_point(size = 2) +
  theme_classic() +
  xlab("d13C_Cor") + 
  ylab("d15N") +
  facet_wrap(~Lake_Name)

##calculating tp and lc 
BSMsm_mixing_model_pred_bound <- function(x){
  df_prep <- x
  LB <- df_prep %>%
    filter(trophic_type == "Littoral Baseline")
  LB_mean_dC <- mean(LB$d13C)
  LB_mean_dN <- mean(LB$d15N)
  PB <- df_prep %>%
    filter(trophic_type == "Pelagic Baseline")
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
#  LC_TP_df_pred$SI_ID <- df_pred$SI_ID
  LC_TP_df_pred
}


##calculating tp and lc off of corrected C 
tp_lc_BSMsm <- split(final_si_df, paste0(final_si_df$Lake_Name)) %>%
  map(BSMsm_mixing_model_pred_bound) %>%
  bind_rows()

BSMsm_mixing_model_pred_bound_2 <- function(x){
  df_prep <- x
  LB <- df_prep %>%
    filter(trophic_type == "Littoral Baseline")
  LB_mean_dC <- mean(LB$d13C_Cor)
  LB_mean_dN <- mean(LB$d15N)
  PB <- df_prep %>%
    filter(trophic_type == "Pelagic Baseline")
  PB_mean_dC <- mean(PB$d13C_Cor)
  PB_mean_dN <- mean(PB$d15N)
  df_pred <- df_prep %>%
    filter(Trophic_Group == "LakeTrout")
  for(i in 1:nrow(df_pred)){
    LC_pred = ((df_pred$d13C_Cor - PB_mean_dC)/(LB_mean_dC - PB_mean_dC))
    LC_pred[LC_pred>1] <- 0.999
    LC_pred[LC_pred<0] <- 0.001
    TP_pred = 2 + (((LC_pred * ((df_pred$d15N - LB_mean_dN)/3.4)))+((1-LC_pred)*((df_pred$d15N - PB_mean_dN)/3.4)))
    dN_pred = df_pred$d15N - (((1-LC_pred)*PB_mean_dN)+(LC_pred*LB_mean_dN))
    LC_TP_df_pred <- cbind(LC_pred, TP_pred, dN_pred)
  }
  LC_TP_df_pred <- as.data.frame(LC_TP_df_pred)
  LC_TP_df_pred$Trophic_Group <- df_pred$Trophic_Group
  LC_TP_df_pred$Lake_Name <- df_pred$Lake_Name
 # LC_TP_df_pred$SI_ID <- df_pred$SI_ID
  LC_TP_df_pred
}

tp_lc_BSMsm <- split(final_si_df, paste0(final_si_df$Lake_Name)) %>%
  map(BSMsm_mixing_model_pred_bound_2) %>%
  bind_rows()



##read in biotic and abiotic data
abiotic_df <- read.csv("data/abiotic_dat_july31.csv")
biotic_df <- read.csv("data/bioticdat_aug9.csv")



df <- left_join(tp_lc_BSMsm, abiotic_df, by = "Lake_Name") %>%
  left_join(biotic_df, by = "Lake_Name")

df_summary <- df %>%
  group_by(Lake_Name) %>%
  summarise_at(vars(LC_pred, TP_pred, Area_ha, summer_air_temp, LaTro_CUE, total_pred_CUE, sampling_year), list(mean = mean, sd = sd)) %>%
  mutate(log_area_ha = log(Area_ha_mean),
         log_pred_cue = log(total_pred_CUE_mean),
         log_lt_cue = log(LaTro_CUE_mean))


ggplot(df_summary, aes(x = summer_air_temp_mean, y= LC_pred_mean)) +
  geom_point()

ggplot(df_summary, aes(x = log_area_ha, y= LC_pred_mean)) +
  geom_point()


ggplot(df_summary, aes(x = summer_air_temp_mean, y= TP_pred_mean)) +
  geom_point()

ggplot(df_summary, aes(x = log(Area_ha_mean), y= TP_pred_mean)) +
  geom_point()


lm1 <- lm(LC_pred_mean ~ summer_air_temp_mean + log_area_ha + log_lt_cue, data = df_summary)
summary(lm1)

lm2 <- lm(TP_pred_mean ~ summer_air_temp_mean + log_area_ha + log_lt_cue, data = df_summary)
summary(lm2)

gamm1 <- gam(LC_pred_mean ~ summer_air_temp_mean + log_area_ha + s(sampling_year_mean, bs = "re"),
             data = df_summary, method = "REML")
summary(gamm1)

library(gratia)
library(visreg)



lc_air_1 <- visreg(gamm1, "summer_air_temp_mean", partial = TRUE, gg = TRUE)
lc_air_1 <- lc_air_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Mean Summer Air Temp", y = "Mean % Coupling")
lc_air_1

lc_area_1 <- visreg(gamm1, "log_area_ha", partial = TRUE, gg = TRUE)
lc_area_1 <- lc_area_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Lake Area (log)", y = "Mean % Coupling")
lc_area_1

gamm1.1 <- gam(LC_pred_mean ~ summer_air_temp_mean + log_area_ha + log_lt_cue + s(sampling_year_mean, bs = "re"),
             data = df_summary, method = "REML")
summary(gamm1.1)
visreg(gamm1.1, "summer_air_temp_mean", partial = TRUE)
visreg(gamm1.1, "log_area_ha", partial = TRUE)
visreg(gamm1.1, "log_lt_cue", partial = TRUE)

gamm2 <- gam(TP_pred_mean ~ summer_air_temp_mean + log_area_ha + s(sampling_year_mean, bs = "re"),
             data = df_summary, method = "REML")
summary(gamm2)
visreg(gamm2, "summer_air_temp_mean", partial = TRUE)
visreg(gamm2, "log_area_ha", partial = TRUE)

tp_air_1 <- visreg(gamm2, "summer_air_temp_mean", partial = TRUE, gg = TRUE)
tp_air_1 <- tp_air_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Mean Summer Air Temp", y = "Mean Trophic Position")
tp_air_1

tp_area_1 <- visreg(gamm2, "log_area_ha", partial = TRUE, gg = TRUE)
tp_area_1 <- tp_area_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Lake Area (log)", y = "Mean Trophic Position")
tp_area_1

mean_response_plot <- ggarrange(lc_area_1, lc_air_1, tp_area_1, tp_air_1, nrow = 2, ncol = 2)
mean_response_plot

gamm2.1 <- gam(TP_pred_mean ~ summer_air_temp_mean + log_area_ha + log_lt_cue + s(sampling_year_mean, bs = "re"),
               data = df_summary, method = "REML")
summary(gamm2.1)

visreg(gamm2.1, "summer_air_temp_mean", partial = TRUE)
visreg(gamm2.1, "log_area_ha", partial = TRUE)
visreg(gamm2.1, "log_lt_cue", partial = TRUE)


##need to think about competition stats, because changes effect of temp when you include lt cue in the model



ggplot(df_summary, aes(x = log(LaTro_CUE_mean), y= LC_pred_mean)) +
  geom_point()

ggplot(df_summary, aes(x = log(LaTro_CUE_mean), y= TP_pred_mean)) +
  geom_point()

ggplot(df_summary, aes(x = log(total_pred_CUE_mean), y= TP_pred_mean)) +
  geom_point()

ggplot(df_summary, aes(x = log(total_pred_CUE_mean), y= LC_pred_mean)) +
  geom_point()

##looking at correlation between predictor variables
ggplot(df_summary, aes(x = summer_air_temp_mean, y=log_area_ha)) +
  geom_point()
lm <- lm(log_area_ha ~ summer_air_temp_mean, data = df_summary)
summary(lm)
##not significantly related 

ggplot(df_summary, aes(x = summer_air_temp_mean, y=log_pred_cue)) +
  geom_point()
lm <- lm(log_pred_cue ~ summer_air_temp_mean, data = df_summary)
summary(lm)
#not significantly related 

ggplot(df_summary, aes(x = log_area_ha, y=log_pred_cue)) +
  geom_point()
lm <- lm(log_pred_cue ~ log_area_ha, data = df_summary)
summary(lm)
##is significantly correlated 

ggplot(df_summary, aes(x = log_area_ha, y=log_lt_cue)) +
  geom_point()
lm <- lm(log_lt_cue ~ log_area_ha, data = df_summary)
summary(lm)
##not significantly correlated 

ggplot(df_summary, aes(x = summer_air_temp_mean, y=log_lt_cue)) +
  geom_point()
lm <- lm(log_lt_cue ~ summer_air_temp_mean, data = df_summary)
summary(lm)
##not significantly correlated 





##siber analysis
#load packages
library(SIBER)



siber_df <- tp_lc_BSMsm %>%
  mutate(log_LC = log(LC_pred),
         log_TP = log(TP_pred))
#check how many lakes are in this df
unique(siber_df$Lake_Name)
#siber_df <- final_si_df %>%
#  filter(Trophic_Group == "LakeTrout")


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

#siber_df1 <- siber_df %>%
#  dplyr::select(d13C, d15N, group, community)

names(siber_df1)[1] <- "iso1" 
names(siber_df1)[2] <- "iso2"
##"iso1" - LC and "iso2" - TP

siber_df1$group <- as.integer(siber_df1$group)

str(siber_df1)
##Testing normal distribution
library(mvnormtest)
shapiro.test(siber_df1$iso1)
shapiro.test(siber_df1$iso2)

hist(siber_df1$iso1)
hist(siber_df1$iso2)

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

#Make ellipse plot in ggplot
siber_cc_ggplot <- siber_df1 %>% mutate(group = factor(group), 
                                          community = factor(community),
                                          TC_pred = iso1, 
                                          TP_pred = iso2,
                                          .keep = "unused")

first.plot <- ggplot(data = siber_cc_ggplot, 
                     aes(x = TC_pred, 
                         y = TP_pred)) + 
  geom_point(aes(color = community, shape = group), size = 5) +
  ylab("Trophic Position") +
  xlab("% Littoral Coupling") + 
  theme(text = element_text(size=16)) + 
#  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) + 
  theme_classic()
first.plot

# decide how big an ellipse you want to draw
#p.ell <- pchisq(1,2) ##use this for a standard ellipse
p.ell <- pchisq(1,2)
ellipse.plot <- first.plot + 
  stat_ellipse(aes(group = interaction(group, community), 
                   fill = community,
                   color = community), 
               alpha = 0.25, 
               level = p.ell,
               type = "norm",
               geom = "polygon") +
#  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
  theme(axis.text.x = element_text(hjust = 1, size = 12),axis.text.y = element_text(size = 10),axis.title.y=element_text(size = 12), axis.title.x = element_text(size = 12), legend.position = "right", text = element_text(family = "Times New Roman")) 

ellipse.plot








#create df of group metrics
group_metrics_df <- as.data.frame(group.ML)

# Add row names and pivot wider
group_metrics_df <- slice(group_metrics_df, 3)

group_metrics_df <- pivot_longer(group_metrics_df, cols = everything(), names_to = "Lake_Name", values_to = "SEAc")

# Remove ".1" from Lake_Name
group_metrics_df <- group_metrics_df %>%
  mutate(Lake_Name = str_replace(Lake_Name, "\\.1$", ""))

##why does pipestone have SEAc of 0? 

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







##look at relationships w/ temp and lake size 
siber_df_final <- left_join(SIBER_df_final, abiotic_df, by = "Lake_Name") %>%
  left_join(biotic_df, by = "Lake_Name") %>%
  mutate(log_area_ha = log(Area_ha),
         log_lt_cue = log(LaTro_CUE),
         log_pred_cue = log(total_pred_CUE),
         log_dX_range = log(dX_range),
         log_TP_range = log(dY_range),
         log_SEAc = log(SEAc),
         log_SDNND = log(SDNND))

##look at correlation matrix 
library(ggcorrplot)

pred_vars <- siber_df_final %>%
  select(log_area_ha, log_lt_cue, log_pred_cue, summer_air_temp) %>%
  filter(!is.na(log_lt_cue))
pred_vars_cor <- round(cor(pred_vars),1)
ggcorrplot(pred_vars_cor, hc.order = TRUE, type = "lower", lab = TRUE, outline.col = "white")

ggplot(siber_df_final, aes(x = summer_air_temp, y= log_dX_range)) +
  geom_point()

ggplot(siber_df_final, aes(x = log_area_ha, y= log_dX_range)) +
  geom_point()

ggplot(siber_df_final, aes(x = summer_air_temp, y= log_TP_range)) +
  geom_point()


ggplot(siber_df_final, aes(x = log_area_ha, y= log_TP_range)) +
  geom_point()

ggplot(siber_df_final, aes(x = log(LaTro_CUE), y= log_dX_range)) +
  geom_point()

ggplot(siber_df_final, aes(x = log(LaTro_CUE), y= log_TP_range)) +
  geom_point()

ggplot(siber_df_final, aes(x = log(total_pred_CUE), y= log_dX_range)) +
  geom_point()
ggplot(siber_df_final, aes(x = log(total_pred_CUE), y= log_TP_range)) +
  geom_point()


library(car)
##pipestone has 0 so cant log 
##
siber_df_final <- siber_df_final %>%
  filter(Lake_Name != "Pipestone")

lm1 <- lm(log_dX_range~ summer_air_temp + log_area_ha, data = siber_df_final)
summary(lm1)

lm1 <- lm(log_dX_range~ summer_air_temp + log_area_ha + log_lt_cue, data = siber_df_final)
summary(lm1)
vif(lm1)

lm2 <- lm(log_dX_range ~ summer_air_temp + log_area_ha + log_pred_cue, data = siber_df_final)
summary(lm2)


lm3 <- lm(SEAc ~ summer_air_temp + log_area_ha + log_pred_cue, data = siber_df_final)
summary(lm3)


lm4 <- lm(TA ~ summer_air_temp + log_area_ha + log_pred_cue, data = siber_df_final)

summary(lm4)

lm5 <- lm(SDNND ~ summer_air_temp + log_area_ha + log_pred_cue, data = siber_df_final)
summary(lm5)

lm6 <- lm(log_SDNND ~ summer_air_temp + log_area_ha, data = siber_df_final)
summary(lm6)

lm6 <- lm(log_SDNND ~ summer_air_temp + log_area_ha + log_lt_cue, data = siber_df_final)
summary(lm6)

library(mgcv)

gamm1 <- gam(log_dX_range ~ summer_air_temp + log_area_ha + s(sampling_year, bs = "re"),
             data = siber_df_final, method = "REML")
summary(gamm1)
lc_range_air_1 <- visreg(gamm1, "summer_air_temp", partial = TRUE, gg = TRUE)
lc_range_air_1 <- lc_range_air_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Mean Summer Air Temp", y = "Coupling Range (log)")
lc_range_air_1

lc_range_area_1 <- visreg(gamm1, "log_area_ha", partial = TRUE, gg = TRUE)

lc_range_area_1 <- lc_range_area_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Lake Area (log)", y = "Coupling Range (log)")
lc_range_area_1


ltcue_lakes <- siber_df_final %>%
  filter(!is.na(log_lt_cue)) %>%
  select(Lake_Name)

gamm1.1 <- gam(log_dX_range ~ summer_air_temp + log_area_ha + log_pred_cue + s(sampling_year, bs = "re"),
             data = siber_df_final, method = "REML")
summary(gamm1.1)

gamm1.2 <- gam(log_dX_range ~ summer_air_temp + log_area_ha + log_lt_cue + log_pred_cue + s(sampling_year, bs = "re"),
               data = siber_df_final, method = "REML")
summary(gamm1.2)

lc_range_air_2 <- visreg(gamm1.2, "summer_air_temp", partial = TRUE, gg = TRUE)
lc_range_air_2 <- lc_range_air_2 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Mean Summer Air Temp", y = "Coupling Range (log)")
lc_range_air_2

lc_range_area_2 <- visreg(gamm1.2, "log_area_ha", partial = TRUE, gg = TRUE)

lc_range_area_2 <- lc_range_area_2 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Lake Area (log)", y = "Coupling Range (log)")
lc_range_area_2

lc_range_lt_1 <- visreg(gamm1.2, "log_lt_cue", partial = TRUE, gg = TRUE)
lc_range_lt_1 <- lc_range_lt_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Lake Trout CPUE (log)", y = "Coupling Range (log)")
lc_range_lt_1

lc_range_pred_1 <- visreg(gamm1.2, "log_pred_cue", partial = TRUE, gg = TRUE)
lc_range_pred_1 <- lc_range_pred_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Other Predator CPUE (log)", y = "Coupling Range (log)")
lc_range_pred_1


##testing plotting them on the same figure
library(visreg)
library(dplyr)
library(ggplot2)

# Extract visreg data
vis_lt <- visreg(gamm1.2, "log_lt_cue", partial = TRUE, plot = FALSE)
vis_pred <- visreg(gamm1.2, "log_pred_cue", partial = TRUE, plot = FALSE)

# Convert to dataframes and label
df_lt <- vis_lt$fit %>%
  mutate(variable = "Lake Trout CPUE", cpue = vis_lt$fit$log_lt_cue)

df_pred <- vis_pred$fit %>%
  mutate(variable = "Other Predator CPUE", cpue = vis_pred$fit$log_pred_cue)

# Combine
df_combined <- bind_rows(df_lt, df_pred)

# Optional: partial residuals
res_lt <- vis_lt$res %>%
  mutate(variable = "Lake Trout CPUE", cpue = vis_lt$res$log_lt_cue)

res_pred <- vis_pred$res %>%
  mutate(variable = "Other Predator CPUE", cpue = vis_pred$res$log_pred_cue)

res_combined <- bind_rows(res_lt, res_pred)

# Plot
ggplot() +
  geom_point(data = res_combined, aes(x = cpue, y = visregRes, color = variable), alpha = 0.5, size = 2.5) +
  geom_line(data = df_combined, aes(x = cpue, y = visregFit, color = variable), linewidth = 1.2) +
  geom_ribbon(data = df_combined, aes(x = cpue, ymin = visregLwr, ymax = visregUpr, fill = variable), alpha = 0.2) +
  theme_classic() +
  labs(x = "CPUE (log)", y = "Coupling Range (log)", color = "Predator Type", fill = "Predator Type")



gamm2 <- gam(log_TP_range ~ summer_air_temp + log_area_ha + s(sampling_year, bs = "re"),
             data = siber_df_final, method = "REML")
summary(gamm2)
tp_range_air_1 <- visreg(gamm2, "summer_air_temp", partial = TRUE, gg = TRUE)
tp_range_air_1 <- tp_range_air_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Mean Summer Air Temp", y = "Trophic Position Range (log)")
tp_range_air_1

tp_range_area_1 <- visreg(gamm2, "log_area_ha", partial = TRUE, gg = TRUE)
tp_range_area_1 <- tp_range_area_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Lake Area (log)", y = "Trophic Position Range (log)")
tp_range_area_1

tp_range_lt_1 <- visreg(gamm2, "log_lt_cue", partial = TRUE, gg = TRUE)
tp_range_lt_1 <- tp_range_lt_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Lake Trout CPUE (log)", y = "Trophic Position Range (log)")
tp_range_lt_1


gamm2.1 <- gam(log_TP_range ~ summer_air_temp + log_area_ha + log_lt_cue + s(sampling_year, bs = "re"),
             data = siber_df_final, method = "REML")
summary(gamm2.1)
vif()


tp_range_air_2 <- visreg(gamm2.1, "summer_air_temp", partial = TRUE, gg = TRUE)
tp_range_air_2 <- tp_range_air_2 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Mean Summer Air Temp", y = "TP Range (log)")
tp_range_air_2

tp_range_area_2 <- visreg(gamm2.1, "log_area_ha", partial = TRUE, gg = TRUE)
tp_range_area_2 <- tp_range_area_2 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Lake Area (log)", y = "TP Range (log)")
tp_range_area_2

tp_range_lt_1 <- visreg(gamm2.1, "log_lt_cue", partial = TRUE, gg = TRUE)
tp_range_lt_1 <- tp_range_lt_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Lake Trout CPUE (log)", y = "TP Range (log)")
tp_range_lt_1

ind_var_comp <- ggarrange(lc_range_area_2, lc_range_air_2, lc_range_lt_1, tp_range_area_2, tp_range_air_2, tp_range_lt_1, nrow = 2, ncol = 3)
ind_var_comp


gamm3 <- gam(log_SEAc ~ summer_air_temp + log_area_ha + s(sampling_year, bs = "re"),
             data = siber_df_final, method = "REML")
summary(gamm3)
sea_range_air_1 <- visreg(gamm3, "summer_air_temp", partial = TRUE, gg = TRUE)
sea_range_air_1 <- sea_range_air_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Mean Summer Air Temp", y = "SEAc (log)")
sea_range_air_1

sea_range_area_1 <- visreg(gamm3, "log_area_ha", partial = TRUE, gg = TRUE)
sea_range_area_1 <- sea_range_area_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Lake Area (log)", y = "SEAc (log)")
sea_range_area_1


gamm3.1 <- gam(log_SEAc ~ summer_air_temp + log_area_ha + log_lt_cue + log_pred_cue + s(sampling_year, bs = "re"),
             data = siber_df_final, method = "REML")
summary(gamm3.1)

gamm4 <- gam(log_SDNND ~ summer_air_temp + log_area_ha + s(sampling_year, bs = "re"),
             data = siber_df_final, method = "REML")
summary(gamm4)

sdnnd_range_air_1 <- visreg(gamm4, "summer_air_temp", partial = TRUE, gg = TRUE)
sdnnd_range_air_1 <- sdnnd_range_air_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Mean Summer Air Temp", y = "SDNND (log)")
sdnnd_range_air_1

sdnnd_range_area_1 <- visreg(gamm4, "log_area_ha", partial = TRUE, gg = TRUE)
sdnnd_range_area_1 <- sdnnd_range_area_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Lake Area (log)", y = "SDNND (log)")
sdnnd_range_area_1


gamm4.1 <- gam(log_SDNND ~ summer_air_temp + log_area_ha + log_lt_cue + log_pred_cue+ s(sampling_year, bs = "re"),
             data = siber_df_final, method = "REML")
summary(gamm4.1)

library(ggpubr)
##put some of these plots together
ind_var_plot_1 <- ggarrange(lc_range_area_1, lc_range_air_1, tp_range_area_1, tp_range_air_1, nrow = 2, ncol = 2)
ind_var_plot_1

ind_var_plot_2 <- ggarrange(sea_range_area_1, sea_range_air_1, sdnnd_range_area_1, sdnnd_range_air_1, nrow = 2, ncol = 2)
ind_var_plot_2

library(lme4)
library(lmerTest)

#lmm1 <- lmer(dX_range ~ summer_air_temp + log_area_ha + (1 | sampling_year), data = siber_df_final)
#summary(lmm1)
##don't have hierarchical structure, so mixed effects doesnt make sense 

##what roughly as first go does competition data look like?

##look at differences in temp across latitude/longitude cluster
ggplot(abiotic_df, aes(x = Latitude, y = summer_air_temp)) +
  geom_point()

ggplot(abiotic_df, aes(x = Longitude, y = summer_air_temp)) +
  geom_point()


##trying out a map;
library(sf)
library(mapview)

sites_coord <- st_as_sf(abiotic_df, coords = c("Longitude", "Latitude"), crs = 4326)
mapview(sites_coord, zcol = "summer_air_temp", col.regions = colorRampPalette(c("blue", "red"))(10), legend = TRUE)
cex = "Area_ha", 
